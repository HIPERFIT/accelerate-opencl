{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      : Data.Array.Accelerate.OpenCL.CodeGen.Reduce
-- Copyright   : [2011] Martin Dybdal
-- License     : BSD3
--
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Constructors for array computation skeletons
--

module Data.Array.Accelerate.OpenCL.CodeGen.Reduce
  (
    mkFold, mkFold1
 --, mkFoldSeg, mkFold1Seg
  )
  where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

import qualified Language.C as C
import qualified Language.C.Syntax
import qualified Data.Loc
import qualified Data.Symbol

import Language.C.Quote.OpenCL

import Data.Array.Accelerate.OpenCL.CodeGen.Data
import Data.Array.Accelerate.OpenCL.CodeGen.Util
import Data.Array.Accelerate.OpenCL.CodeGen.Tuple
import Data.Array.Accelerate.OpenCL.CodeGen.Monad


-- Exported functions
-- ------------------
mkFold :: ([C.Type],Int) -> C.Exp -> C.Exp -> CUTranslSkel
mkFold (ty, dimIn) identity apply = makeFold True (ty, dimIn) (Just identity) apply

mkFold1 :: ([C.Type],Int) -> C.Exp -> CUTranslSkel
mkFold1 (ty, dimIn) apply = makeFold False (ty, dimIn) Nothing apply



-- Reduction
-- ---------

makeFold :: Bool -> ([C.Type],Int) -> Maybe C.Exp -> C.Exp -> CUTranslSkel
makeFold inclusive (ty, dimIn) identity apply = runCGM $ do
  (d_out, d_inA : _) <- mkTupleTypeAsc 1 ty
  (d_local,local_params) <- mkParameterList Local (Just "local") n tynames
  fromMaybe (return ()) (mkIdentity <$> identity)
  mkApplyAsc 2 apply
  mkDim "DimOut" (dimIn-1)
  mkDim "DimInA" dimIn
  let mkSkel | dimIn == 1 = mkFoldAllSkel
             | otherwise  = mkFoldSkel dimIn
  mkSkel d_out d_inA (d_local, local_params) inclusive
    where
      n = length ty
      tynames
        | n > 1     = take n ["TyOut" ++ "_" ++ show i | i <- [0..]]
        | otherwise = ["TyOut"]

mkFoldAllSkel :: Arguments -> Arguments -> (Arguments, [C.Param]) -> Bool -> CGM ()
mkFoldAllSkel d_out d_inA (d_local, local_params) inclusive = do
  ps <- getParams
  mkHandleSeed d_out inclusive
  mkBlockReduce local_params d_local

  addDefinitions
    [cunit|
        __kernel void fold (const typename Ix shape,
                            $params:ps,
                            $params:local_params) {

             /*
              * Calculate first level of reduction reading into shared memory
              */
             const typename Ix   tid       = get_local_id(0);
             const typename Ix   blockSize = get_local_size(0);
             const typename Ix   gridSize  = get_global_size(0);
             typename Ix    i              = get_global_id(0);
             typename TyOut sum;
             
             typename Ix number_of_blocks = gridSize/blockSize;
             typename Ix block_id = get_global_id(0)/blockSize;

             /*
              * Reduce multiple elements per thread. The number is determined by the
              * number of active thread blocks (via gridDim). More blocks will result in
              * a larger `gridSize', and hence fewer elements per thread
              *
              * The loop stride of `gridSize' is used to maintain coalescing.
              */
             if (i < shape) {
                 sum = getA(i, $args:d_inA);
                 for (i += gridSize; i < shape; i += gridSize)
                     sum = apply(sum, getA(i, $args:d_inA));
             }

             /*
              * Each thread puts its local sum into shared memory, then threads
              * cooperatively reduce the shared array to a single value.
              */
             set_local(tid, sum, $args:d_local);
             
             barrier(CLK_LOCAL_MEM_FENCE);

             typename Ix n = blockSize;
             if(block_id == number_of_blocks - 1 && shape % blockSize != 0) {
               // if last block and not all elements of that block should be included
               n = shape % blockSize;
             }

             sum = reduce_block_n(sum, n, $args:d_local);

             /*
              * Write the results of this block back to global memory. If we are the last
              * phase of a recursive multi-block reduction, include the seed element.
              */
             if (tid == 0) {
               handleSeed(shape, sum, $args:d_out, $args:d_inA);
             }
         }
    |]


mkFoldSkel :: Int -> Arguments -> Arguments -> (Arguments, [C.Param]) -> Bool -> CGM ()
mkFoldSkel dimIn d_out d_inA (d_local, local_params) inclusive = do
  ps <- getParams
  let dimOut = dimIn - 1

  mkHandleSeedFold d_out inclusive
  mkWarpReduce local_params d_local

  addDefinitions
    [cunit|
        __kernel void fold(const typename DimOut shOut,
                           const typename DimInA shInA,
                           $params:ps,
                           $params:local_params) {
            int warpSize = 32;

            const typename Ix num_elements = $id:(indexHead dimIn)(shInA);
            const typename Ix num_segments = $id:(size dimOut)(shOut);

            const typename Ix num_vectors  = get_local_size(0) / warpSize * (get_global_size(0) / get_local_size(0));
            const typename Ix thread_id    = get_global_id(0);
            const typename Ix vector_id    = thread_id / warpSize;
            const typename Ix thread_lane  = get_local_id(0) & (warpSize - 1);

            /*
             * Each warp reduces elements along a projection through an innermost
             * dimension to a single value
             */
            for (typename Ix seg = vector_id; seg < num_segments; seg += num_vectors)
            {

                const typename Ix    start = seg   * num_elements;
                const typename Ix    end   = start + num_elements;
                      typename TyOut sum;

                if (num_elements > warpSize)
                {
                    /*
                     * Ensure aligned access to global memory, and that each thread
                     * initialises its local sum.
                     */
                    typename Ix i = start - (start & (warpSize - 1)) + thread_lane;
                    if (i >= start)
                        sum = getA(i, $args:d_inA);

                    if (i + warpSize < end)
                    {
                        typename TyOut tmp = getA(i + warpSize, $args:d_inA);

                        if (i >= start) sum = apply(sum, tmp);
                        else            sum = tmp;
                    }

                    /*
                     * Now, iterate along the inner-most dimension collecting a local sum
                     */
                    for (i += 2 * warpSize; i < end; i += warpSize)
                        sum = apply(sum, getA(i, $args:d_inA));
                }
                else if (start + thread_lane < end)
                {
                    sum = getA(start + thread_lane, $args:d_inA);
                }

                /*
                 * Each thread puts its local sum into shared memory, then cooperatively
                 * reduce the shared array to a single value.
                 */
                set_local(get_local_id(0), sum, $args:d_local);
                sum = reduce_warp_n(sum, min(num_elements, warpSize), $args:d_local);

                /*
                 * Finally, the first thread writes the result for this segment
                 */
                if (thread_lane == 0) {
                  handleSeed(seg, num_elements, sum, $args:d_out, $args:d_inA);
                }
            }
        }
    |]

-- | Cooperatively reduce a single warp's segment of an array to a single value
mkWarpReduce :: [C.Param] -> Arguments -> CGM ()
mkWarpReduce ps args = do
  addDefinitions $
    [cunit|
       inline typename TyOut reduce_warp_n (typename TyOut sum,
                                            typename Ix n,
                                            $params:ps) {
         int warpSize = 32;
         const typename Ix tid  = get_local_id(0);
         const typename Ix lane = get_local_id(0) & (warpSize - 1);

         if (n > 16 && lane + 16 < n) { sum = apply(sum, getA_local(tid+16, $args:args)); set_local(tid, sum, $args:args); }
         if (n >  8 && lane +  8 < n) { sum = apply(sum, getA_local(tid+ 8, $args:args)); set_local(tid, sum, $args:args); }
         if (n >  4 && lane +  4 < n) { sum = apply(sum, getA_local(tid+ 4, $args:args)); set_local(tid, sum, $args:args); }
         if (n >  2 && lane +  2 < n) { sum = apply(sum, getA_local(tid+ 2, $args:args)); set_local(tid, sum, $args:args); }
         if (n >  1 && lane +  1 < n) { sum = apply(sum, getA_local(tid+ 1, $args:args)); }
         return sum;
       }
    |]

-- | Block reduction to a single value
mkBlockReduce :: [C.Param] -> Arguments -> CGM ()
mkBlockReduce ps args = do
  addDefinitions $
    [cunit|
       inline typename TyOut reduce_block_n(typename TyOut sum,
                                            typename Ix n,
                                            $params:ps) {
         const typename Ix tid = get_local_id(0);
         if (n > 512) { if (tid < 512 && tid + 512 < n) { sum = apply(sum, getA_local(tid+512, $args:args)); set_local(tid, sum, $args:args); } }
         barrier(CLK_LOCAL_MEM_FENCE);
         if (n > 256) { if (tid < 256 && tid + 256 < n) { sum = apply(sum, getA_local(tid+256, $args:args)); set_local(tid, sum, $args:args); } }
         barrier(CLK_LOCAL_MEM_FENCE);
         if (n > 128) { if (tid < 128 && tid + 128 < n) { sum = apply(sum, getA_local(tid+128, $args:args)); set_local(tid, sum, $args:args); } }
         barrier(CLK_LOCAL_MEM_FENCE);
         if (n >  64) { if (tid <  64 && tid +  64 < n) { sum = apply(sum, getA_local(tid+ 64, $args:args)); set_local(tid, sum, $args:args); } }
         barrier(CLK_LOCAL_MEM_FENCE);
         if (n >  32) { if (tid <  32 && tid +  32 < n) { sum = apply(sum, getA_local(tid+ 32, $args:args)); set_local(tid, sum, $args:args); }}
         barrier(CLK_LOCAL_MEM_FENCE);
         if (n >  16) { if (tid <  16 && tid +  16 < n) { sum = apply(sum, getA_local(tid+ 16, $args:args)); set_local(tid, sum, $args:args); }}
         barrier(CLK_LOCAL_MEM_FENCE);
         if (n >  8) { if (tid <  8 && tid +     8 < n) { sum = apply(sum, getA_local(tid+  8, $args:args)); set_local(tid, sum, $args:args); }}
         barrier(CLK_LOCAL_MEM_FENCE);
         if (n >  4) { if (tid <  4 && tid +     4 < n) { sum = apply(sum, getA_local(tid+  4, $args:args)); set_local(tid, sum, $args:args); }}
         barrier(CLK_LOCAL_MEM_FENCE);
         if (n >  2) { if (tid <  2 && tid +     2 < n) { sum = apply(sum, getA_local(tid+  2, $args:args)); set_local(tid, sum, $args:args); }}
         barrier(CLK_LOCAL_MEM_FENCE);
         if (n >  1) { if (tid == 0 && tid +     1 < n) { sum = apply(sum, getA_local(tid+  1, $args:args)); }}

         return sum;
       }
    |]


mkHandleSeed :: Arguments -> Bool -> CGM ()
mkHandleSeed d_out False = do
  ps <- getParams
  addDefinitions
    [cunit|
      inline void handleSeed(const typename Ix shape,
                             typename TyOut sum,
                             $params:ps)
      {
          typename Ix blockIdx = (get_global_id(0)-get_local_id(0)) / get_local_size(0);
          set(blockIdx, sum, $args:d_out);
      }
    |]
mkHandleSeed d_out True = do
  ps <- getParams
  addDefinitions
    [cunit|
      inline void handleSeed(const typename Ix shape,
                             typename TyOut sum,
                             $params:ps)
      {
          typename Ix blockIdx = (get_global_id(0)-get_local_id(0)) / get_local_size(0);
          if (shape > 0) {
              typename TyOut seed = get_global_size(0) == get_local_size(0)
                                      ? apply(sum, identity())
                                      : sum ;
              set(blockIdx, seed, $args:d_out);
          }
          else
              set(blockIdx, identity(), $args:d_out);
      }
    |]


mkHandleSeedFold :: Arguments -> Bool -> CGM ()
mkHandleSeedFold d_out False = do
  ps <- getParams
  addDefinitions
    [cunit|
      inline void handleSeed(typename Ix seg,
                             const typename Ix num_elements,
                             typename TyOut sum,
                             $params:ps)
      {
          set(seg, sum, $args:d_out);
      }
    |]
mkHandleSeedFold d_out True = do
  ps <- getParams
  addDefinitions
    [cunit|
      inline void handleSeed(typename Ix seg,
                             const typename Ix num_elements,
                             typename TyOut sum,
                             $params:ps)
      {
          sum = num_elements > 0 ? apply(sum, identity()) : identity();
          set(seg, sum, $args:d_out);
      }
    |]


-- mkFoldSeg :: ([CType],Int) -> [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
-- mkFoldSeg (ty,dim) int identity apply = CUTranslSkel code [] skel
--   where
--     skel = "foldSeg.inl"
--     code = CTranslUnit
--             ( mkTupleTypeAsc 2 ty ++
--             [ mkTuplePartition "ArrOut" ty True
--             , mkIdentity identity
--             , mkApply 2 apply
--             , mkTypedef "Int" False False (head int)
--             , mkDim "DimIn0" dim
--             , mkDim "DimOut" dim ])
--             (mkNodeInfo (initPos skel) (Name 0))

-- mkFold1Seg :: ([CType],Int) -> [CType] -> [CExpr] -> CUTranslSkel
-- mkFold1Seg (ty,dim) int apply = CUTranslSkel code inc skel
--   where
--     skel = "foldSeg.inl"
--     inc  = [(internalIdent "INCLUSIVE", Just (fromBool True))]
--     code = CTranslUnit
--             ( mkTupleTypeAsc 2 ty ++
--             [ mkTuplePartition "ArrOut" ty True
--             , mkApply 2 apply
--             , mkTypedef "Int" False False (head int)
--             , mkDim "DimIn0" dim
--             , mkDim "DimOut" dim ])
--             (mkNodeInfo (initPos skel) (Name 0))

