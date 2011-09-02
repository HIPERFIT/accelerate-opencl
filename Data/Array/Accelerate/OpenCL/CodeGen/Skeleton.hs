{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      : Data.Array.Accelerate.OpenCL.CodeGen.Skeleton
-- Copyright   : [2011] Martin Dybdal
-- License     : BSD3
--
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Constructors for array computation skeletons
--

module Data.Array.Accelerate.OpenCL.CodeGen.Skeleton
  (
    mkGenerate,
  -- mkFold, mkFold1, mkFoldSeg, mkFold1Seg,
    mkMap, mkZipWith,
--    mkStencil, mkStencil2,
--    mkScanl, mkScanr, mkScanl', mkScanr', mkScanl1, mkScanr1,
    mkPermute, mkBackpermute, mkReplicate
--, mkIndex
  )
  where

import qualified Language.C as C
import Language.C.Syntax
import Language.C.Quote.OpenCL

import Data.Loc
import Data.Symbol


--import System.FilePath
--import Data.Array.Accelerate.Type
import Data.Array.Accelerate.OpenCL.CodeGen.Data
import Data.Array.Accelerate.OpenCL.CodeGen.Util
import Data.Array.Accelerate.OpenCL.CodeGen.Tuple
import Data.Array.Accelerate.OpenCL.CodeGen.Monad
--import Data.Array.Accelerate.CUDA.CodeGen.Stencil


-- -- Construction
-- -- ------------

mkGenerate :: ([C.Type],Int) -> C.Exp -> CUTranslSkel
mkGenerate (tyOut, dimOut) apply = runCGM $ do
    d_out <- mkOutputTuple tyOut
    shape_out <- mkShape "DimOut" dimOut
    _ <- mkShape "TyInA" dimOut

    mkApply 1 apply

    ps <- getParams
    addDefinitions
      [cunit|
         __kernel void permute (const $ty:shape_out shOut,
                                $params:ps) {
             const $ty:ix n = $id:(size dimOut)(shInA);
             const $ty:ix gridSize  = get_global_size(0);

             for ($ty:ix ix = get_global_id(0); ix < shapeSize; ix += gridSize) {
                 $ty:outType val = apply($id:(fromIndex dimOut)(shOut, ix));
                 set(ix, val, $args:d_out);
             }
         }
      |]

-- Reduction
-- ---------

-- mkFold :: ([CType],Int) -> [CExpr] -> [CExpr] -> CUTranslSkel
-- mkFold (ty,dim) identity apply = CUTranslSkel code [] skel
--   where
--     skel | dim == 1  = "foldAll.inl"
--          | otherwise = "fold.inl"
--     code = CTranslUnit
--             ( mkTupleTypeAsc 2 ty ++
--             [ mkTuplePartition "ArrOut" ty True
--             , mkIdentity identity
--             , mkApply 2 apply
--             , mkDim "DimIn0" dim
--             , mkDim "DimOut" (dim-1) ])
--             (mkNodeInfo (initPos skel) (Name 0))

-- mkFold1 :: ([CType],Int) -> [CExpr] -> CUTranslSkel
-- mkFold1 (ty,dim) apply = CUTranslSkel code inc skel
--   where
--     skel | dim == 1  = "foldAll.inl"
--          | otherwise = "fold.inl"
--     inc  = [(internalIdent "INCLUSIVE", Just (fromBool True))]
--     code = CTranslUnit
--             ( mkTupleTypeAsc 2 ty ++
--             [ mkTuplePartition "ArrOut" ty True
--             , mkApply 2 apply
--             , mkDim "DimIn0" dim
--             , mkDim "DimOut" (dim-1) ])
--             (mkNodeInfo (initPos skel) (Name 0))

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


-- Map
-- ---
mkMap :: [C.Type] -> [C.Type] -> C.Exp -> CUTranslSkel
mkMap tyOut tyIn_A apply = runCGM $ do
  d_out <- mkOutputTuple tyOut
  d_inA <- mkInputTuple "A" tyIn_A
  mkApply 1 apply

  ps <- getParams
  addDefinitions
    [cunit|
       __kernel void map (const typename Ix shape, $params:ps) {
         const typename Ix gridSize = get_global_size(0);

         for(typename Ix idx = get_global_id(0); idx < shape; idx += gridSize) {
           typename TyInA val = getA(idx, $args:d_inA);
           typename TyOut new = apply(val);
           set(idx, new, $args:d_out);
         }
       }
    |]

mkZipWith :: ([C.Type], Int)
          -> ([C.Type], Int)
          ->([C.Type], Int) -> C.Exp -> CUTranslSkel
mkZipWith (tyOut,dimOut) (tyInB, dimInB) (tyInA, dimInA) apply =
  runCGM $ do
    d_out <- mkOutputTuple tyOut
    d_inA <- mkInputTuple "A" tyInA
    d_inB <- mkInputTuple "B" tyInB
    mkApply 2 apply

    shape_out <- mkShape "DimOut" dimOut
    shape_inB <- mkShape "DimInB" dimInB
    shape_inA <- mkShape "DimInA" dimInA

    ps <- getParams
    addDefinitions
      [cunit|
         __kernel void zipWith (const $ty:shape_out shOut,
                                const $ty:shape_inB shInB,
                                const $ty:shape_inA shInA,
                                $params:ps) {
           const $ty:ix shapeSize = $id:(size dimOut)(shOut);
           const $ty:ix gridSize  = get_global_size(0);

           for ($ty:ix ix = get_global_id(0); ix < shapeSize; ix += gridSize) {
             $ty:ix iA = $id:(toIndex dimInB)(shInB, $id:(fromIndex dimInB)(shOut, ix));
             $ty:ix iB = $id:(toIndex dimInA)(shInA, $id:(fromIndex dimInA)(shOut, ix));

             $ty:outType val = apply(getB(iB, $args:d_inB), getA(iA, $args:d_inA)) ;
             set(ix, val, $args:d_out) ;
           }
         }
      |]


-- -- Stencil
-- -- -------

-- mkStencil :: ([CType], Int)
--           -> [CExtDecl] -> [CType] -> [[Int]] -> Boundary [CExpr]
--           -> [CExpr]
--           -> CUTranslSkel
-- mkStencil (tyOut, dim) stencil0 tyIn0 ixs0 boundary0 apply = CUTranslSkel code [] skel
--   where
--     skel = "stencil.inl"
--     code = CTranslUnit
--             ( stencil0                   ++
--               mkTupleType Nothing  tyOut ++
--             [ mkDim "DimOut" dim
--             , mkDim "DimIn0" dim
--             , head $ mkTupleType (Just 0) tyIn0 -- just the scalar type
--             , mkStencilType 0 (length ixs0) tyIn0 ] ++
--               mkStencilGet 0 boundary0 tyIn0 ++
--             [ mkStencilGather 0 dim tyIn0 ixs0
--             , mkStencilApply 1 apply ] )
--             (mkNodeInfo (initPos skel) (Name 0))


-- mkStencil2 :: ([CType], Int)
--            -> [CExtDecl] -> [CType] -> [[Int]] -> Boundary [CExpr]
--            -> [CExtDecl] -> [CType] -> [[Int]] -> Boundary [CExpr]
--            -> [CExpr]
--            -> CUTranslSkel
-- mkStencil2 (tyOut, dim) stencil1 tyIn1 ixs1 boundary1
--                         stencil0 tyIn0 ixs0 boundary0 apply =
--   CUTranslSkel code [] skel
--   where
--     skel = "stencil2.inl"
--     code = CTranslUnit
--             ( stencil0                   ++
--               stencil1                   ++
--               mkTupleType Nothing  tyOut ++
--             [ mkDim "DimOut" dim
--             , mkDim "DimIn1" dim
--             , mkDim "DimIn0" dim
--             , head $ mkTupleType (Just 0) tyIn0 -- just the scalar type
--             , head $ mkTupleType (Just 1) tyIn1
--             , mkStencilType 1 (length ixs1) tyIn1
--             , mkStencilType 0 (length ixs0) tyIn0 ] ++
--               mkStencilGet 1 boundary1 tyIn1 ++
--               mkStencilGet 0 boundary0 tyIn0 ++
--             [ mkStencilGather 1 dim tyIn1 ixs1
--             , mkStencilGather 0 dim tyIn0 ixs0
--             , mkStencilApply 2 apply ] )
--             (mkNodeInfo (initPos skel) (Name 0))


-- -- Scan
-- -- ----

-- -- TODO: use a fast scan for primitive types
-- --
-- mkExclusiveScan :: Bool -> Bool -> [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
-- mkExclusiveScan isReverse isHaskellStyle ty identity apply = CUTranslSkel code defs skel
--   where
--     skel = "scan.inl"
--     defs = [(internalIdent "REVERSE",       Just (fromBool isReverse))
--            ,(internalIdent "HASKELL_STYLE", Just (fromBool isHaskellStyle))]
--     code = CTranslUnit
--             ( mkTupleTypeAsc 2 ty ++
--             [ mkIdentity identity
--             , mkApply 2 apply ])
--             (mkNodeInfo (initPos (takeFileName skel)) (Name 0))

-- mkInclusiveScan :: Bool -> [CType] -> [CExpr] -> CUTranslSkel
-- mkInclusiveScan isReverse ty apply = CUTranslSkel code [rev] skel
--   where
--     skel = "scan1.inl"
--     rev  = (internalIdent "REVERSE", Just (fromBool isReverse))
--     code = CTranslUnit
--             ( mkTupleTypeAsc 2 ty ++
--             [ mkApply 2 apply ])
--             (mkNodeInfo (initPos (takeFileName skel)) (Name 0))

-- mkScanl, mkScanr :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
-- mkScanl = mkExclusiveScan False True
-- mkScanr = mkExclusiveScan True  True

-- mkScanl', mkScanr' :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
-- mkScanl' = mkExclusiveScan False False
-- mkScanr' = mkExclusiveScan True  False

-- mkScanl1, mkScanr1 :: [CType] -> [CExpr] -> CUTranslSkel
-- mkScanl1 = mkInclusiveScan False
-- mkScanr1 = mkInclusiveScan True


-- -- Permutation
-- -- -----------

mkPermute :: [C.Type] -> Int -> Int -> C.Exp -> C.Exp -> CUTranslSkel
mkPermute ty dimOut dimInA combinefn indexfn = runCGM $ do
    (d_out, d_inA : _) <- mkTupleTypeAsc 2 ty
    shape_out <- mkShape "DimOut" dimOut
    shape_inA <- mkShape "DimInA" dimInA

    mkApply 2 combinefn
    mkProject Forward indexfn

    ps <- getParams
    addDefinitions
      [cunit|
         __kernel void permute (const $ty:shape_out shOut,
                                const $ty:shape_inA shInA,
                                $params:ps) {
             const $ty:ix shapeSize = $id:(size dimInA)(shInA);
             const $ty:ix gridSize  = get_global_size(0);

             for ($ty:ix ix = get_global_id(0); ix < shapeSize; ix += gridSize) {
                 $ty:shape_inA src = $id:(fromIndex dimInA)(shIn0, ix);
                 $ty:shape_out dst = project(src);

                 if (!ignore(dst)) {
                     $ty:ix j = $id:(toIndex dimOut)(shOut, dst);

                     $ty:outType val = apply(getA(j, $args:d_out),
                                             getA(ix, $args:d_inA)) ;
                     set(j, val, $args:d_out) ;
                 }
             }
         }
      |]


mkBackpermute :: [C.Type] -> Int -> Int -> C.Exp -> CUTranslSkel
mkBackpermute ty dimOut dimInA indexFn = runCGM $ do
    (d_out, d_inA : _) <- mkTupleTypeAsc 1 ty
    shape_out <- mkShape "DimOut" dimOut
    shape_inA <- mkShape "DimInA" dimInA

    mkProject Backward indexFn

    ps <- getParams
    addDefinitions
      [cunit|
         __kernel void permute (const $ty:shape_out shOut,
                                const $ty:shape_inA shInA,
                                $params:ps) {
             const $ty:ix shapeSize = $id:(size dimInA)(shInA);
             const $ty:ix gridSize  = get_global_size(0);

             for ($ty:ix ix = get_global_id(0); ix < shapeSize; ix += gridSize) {
                 $ty:shape_out src = $id:(fromIndex dimOut)(shOut, ix);
                 $ty:shape_inA src = project(dst);

                 $ty:ix j = $id:(toIndex dimInA)(shInA, dst);
                 set(ix, getA(j, $args:d_inA), $args:d_out) ;
             }
         }
      |]


-- -- Multidimensional Index and Replicate
-- -- ------------------------------------

-- mkIndex :: [CType] -> Int -> Int -> Int -> [CExpr] -> CUTranslSkel
-- mkIndex ty dimSl dimCo dimIn0 slix = CUTranslSkel code [] skel
--   where
--     skel = "slice.inl"
--     code = CTranslUnit
--             ( mkTupleTypeAsc 1 ty ++
--             [ mkDim "Slice"    dimSl
--             , mkDim "CoSlice"  dimCo
--             , mkDim "SliceDim" dimIn0
--             , mkSliceIndex slix ])
--             (mkNodeInfo (initPos skel) (Name 0))

mkReplicate :: [C.Type] -> Int -> Int -> C.Exp -> CUTranslSkel
mkReplicate ty dimSl dimOut slix = runCGM $ do
    (d_out, d_inA : _) <- mkTupleTypeAsc 1 ty
    slice <- mkShape "Slice" dimSl
    slice_dim <- mkShape "SliceDim" dimOut

    mkSliceReplicate slix

    ps <- getParams
    addDefinitions
      [cunit|
         __kernel void permute (const $ty:slice shOut,
                                const $ty:slice_dim shInA,
                                $params:ps) {
             const $ty:ix shapeSize = $id:(size dimOut)(sliceDim);
             const $ty:ix gridSize  = get_global_size(0);

             for ($ty:ix ix = get_global_id(0); ix < shapeSize; ix += gridSize) {
                 $ty:slice_dim dst = $id:(fromIndex dimOut)(sliceDim, ix);
                 $ty:slice src = sliceIndex(dst);

                 $ty:ix j = $id:(toIndex dimSl)(slice, src);
                 set(ix, getA(j, $args:d_inA), $args:d_out) ;
             }
         }
      |]
