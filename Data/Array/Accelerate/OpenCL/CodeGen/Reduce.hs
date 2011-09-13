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
mkFold ty identity apply = makeFold False ty (Just identity) apply

mkFold1 :: ([C.Type],Int) -> C.Exp -> CUTranslSkel
mkFold1 ty apply = makeFold True ty Nothing apply



-- Reduction
-- ---------

makeFold :: Bool -> ([C.Type],Int) -> Maybe C.Exp -> C.Exp -> CUTranslSkel
makeFold inclusive (ty, dim) identity apply = runCGM $ do
  (d_out, d_inA : _) <- mkTupleTypeAsc 1 ty
  fromMaybe (return ()) (mkIdentity <$> identity)
  mkApplyAsc 2 apply
  mkDim "DimInA" dim
  mkDim "DimOut" (dim-1)
  let mkSkel | dim == 1  = mkFoldAllSkel
             | otherwise = mkFoldSkel
  mkSkel d_out d_inA inclusive

mkFoldSkel :: Arguments -> Arguments -> Bool -> CGM ()
mkFoldSkel = error "folds for higher dimensions are not yet supported in the OpenCL backend for Accelerate"

mkFoldAllSkel :: Arguments -> Arguments -> Bool -> CGM ()
mkFoldAllSkel d_out d_inA inclusive = do
  ps <- getParams

  mkHandleSeed d_out inclusive

  let include = "#include <reduce.cl>"
  addDefinitions [cunit| $esc:include |]

  addDefinitions
    [cunit|
        __kernel void fold (const typename Ix shape,
                            $params:ps) {

             volatile __local typename TyOut s_data[100];
             //__global ArrOutTy *s_data = partition(s_ptr, get_local_size(0));

             /*
              * Calculate first level of reduction reading into shared memory
              */
             const typename Ix   tid       = get_local_id(0);
             const typename Ix   blockSize = get_local_size(0);
             const typename Ix   gridSize  = get_global_size(0);
             typename Ix    i              = get_global_id(0);
             typename TyOut sum;

             /*
              * Reduce multiple elements per thread. The number is determined by the
              * number of active thread blocks (via gridDim). More blocks will result in
              * a larger `gridSize', and hence fewer elements per thread
              *
              * The loop stride of `gridSize' is used to maintain coalescing.
              */
             if (i < shape)
             {
                 sum = getA(i, $args:d_inA);
                 for (i += gridSize; i < shape; i += gridSize)
                     sum = apply(sum, getA(i, $args:d_inA));
             }

             /*
              * Each thread puts its local sum into shared memory, then threads
              * cooperatively reduce the shared array to a single value.
              */
             set_local(tid, sum, s_data);
             barrier(CLK_LOCAL_MEM_FENCE);
             sum = reduce_block_n(s_data, sum, min(shape, blockSize));

             /*
              * Write the results of this block back to global memory. If we are the last
              * phase of a recursive multi-block reduction, include the seed element.
              */

             if (tid == 0)
             {
               handleSeed(sum, $args:d_out, $args:d_inA);
             }
         }
    |]


mkHandleSeed :: Arguments -> Bool -> CGM ()
mkHandleSeed d_out False = do
  ps <- getParams
  addDefinitions
    [cunit|
      inline void handleSeed(typename TyOut sum,
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
      inline void handleSeed(typename TyOut sum,
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

