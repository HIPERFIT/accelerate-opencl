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
--    mkGenerate, mkFold, mkFold1, mkFoldSeg, mkFold1Seg,
    mkMap, mkZipWith,
--    mkStencil, mkStencil2,
--    mkScanl, mkScanr, mkScanl', mkScanr', mkScanl1, mkScanr1,
--    mkPermute, mkBackpermute, mkIndex, mkReplicate
  )
  where

import qualified Language.C as C
import Language.C.Syntax
import Language.C.Quote.OpenCL

import Data.Loc
import Data.Symbol


--import System.FilePath
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.OpenCL.CodeGen.Data
import Data.Array.Accelerate.OpenCL.CodeGen.Util
import Data.Array.Accelerate.OpenCL.CodeGen.Tuple
--import Data.Array.Accelerate.CUDA.CodeGen.Stencil


-- -- Construction
-- -- ------------

-- mkGenerate :: ([CType],Int) -> [CExpr] -> CUTranslSkel
-- mkGenerate (tyOut, dimOut) apply = CUTranslSkel code [] skel
--   where
--     skel = "generate.inl"
--     code = CTranslUnit
--             ( mkTupleType Nothing  tyOut ++
--             [ mkDim "DimOut" dimOut
--             , mkDim "TyIn0"  dimOut
--             , mkApply 1 apply ])
--             (mkNodeInfo (initPos skel) (Name 0))


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
mkMap tyOut tyIn_A apply = CUTranslSkel $ outputdefs ++ inputdefs ++ [apply'] ++ skel
  where
    (outputdefs, out_params, Set callSet) = mkOutputTuple tyOut
    (inputdefs, in_params, Get callGet) = mkInputTuple "A" tyIn_A

    skel :: [Definition]
    skel = [cunit|
              __kernel void map (const int shape, $params:(out_params ++ in_params)) {
                int idx;
                const int gridSize = get_global_size(0);

                for(idx = get_global_id(0); idx < shape; idx += gridSize) {
                  $ty:(typename "TyInA") val = $exp:(callGet "idx") ;
                  $ty:outType new = apply(val) ;
                  $exp:(callSet "idx" "new") ;
                }
              }
           |]

    apply' :: Definition
    apply' = mkApply 1 apply


mkZipWith :: ([C.Type], Int)
          -> ([C.Type], Int)
          ->([C.Type], Int) -> C.Exp -> CUTranslSkel
mkZipWith (tyOut,dimOut) (tyInB, dimInB) (tyInA, dimInA) apply =
      CUTranslSkel $ outputdefs ++ inputdefsA ++ inputdefsB ++
                     [sh_out_def, sh_inB_def, sh_inA_def, apply'] ++ skel
  where
    (outputdefs, out_params, Set callSet) = mkOutputTuple tyOut
    (inputdefsA, in_paramsA, Get callGetA) = mkInputTuple "A" tyInA
    (inputdefsB, in_paramsB, Get callGetB) = mkInputTuple "B" tyInB

    (sh_out_type, sh_out_def) = mkShape "DimOut" dimOut
    (sh_inB_type, sh_inB_def) = mkShape "DimInB" dimInB
    (sh_inA_type, sh_inA_def) = mkShape "DimInA" dimInA

    apply' :: Definition
    apply' = mkApply 2 apply

    skel :: [Definition]
    skel = [cunit|
              __kernel void zipWith (const $ty:sh_out_type shOut,
                                     const $ty:sh_inB_type shInB,
                                     const $ty:sh_inA_type shInA,
                                     $params:(out_params ++ in_paramsB ++ in_paramsA)) {
                const $ty:ixType shapeSize = $id:(size dimOut)(shOut);
                const $ty:ixType gridSize  = get_global_size(0);

                for ($ty:ixType ix = get_global_id(0); ix < shapeSize; ix += gridSize) {
                  $ty:ixType iA = $id:(toIndex dimInB)(shInB, $id:(fromIndex dimInB)(shOut, ix));
                  $ty:ixType iB = $id:(toIndex dimInA)(shInA, $id:(fromIndex dimInA)(shOut, ix));

                  $ty:(typename "TyInB") valB = $exp:(callGetB "iB") ;
                  $ty:(typename "TyInA") valA = $exp:(callGetA "iA") ;
                  $ty:outType new = apply(valB, valA) ;
                  $exp:(callSet "ix" "new") ;
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

-- mkPermute :: [CType] -> Int -> Int -> [CExpr] -> [CExpr] -> CUTranslSkel
-- mkPermute ty dimOut dimIn0 combinefn indexfn = CUTranslSkel code [] skel
--   where
--     skel = "permute.inl"
--     code = CTranslUnit
--             ( mkTupleTypeAsc 2 ty ++
--             [ mkDim "DimOut" dimOut
--             , mkDim "DimIn0" dimIn0
--             , mkProject Forward indexfn
--             , mkApply 2 combinefn ])
--             (mkNodeInfo (initPos skel) (Name 0))

-- mkBackpermute :: [CType] -> Int -> Int -> [CExpr] -> CUTranslSkel
-- mkBackpermute ty dimOut dimIn0 indexFn = CUTranslSkel code [] skel
--   where
--     skel = "backpermute.inl"
--     code = CTranslUnit
--             ( mkTupleTypeAsc 1 ty ++
--             [ mkDim "DimOut" dimOut
--             , mkDim "DimIn0" dimIn0
--             , mkProject Backward indexFn ])
--             (mkNodeInfo (initPos skel) (Name 0))


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


-- mkReplicate :: [CType] -> Int -> Int -> [CExpr] -> CUTranslSkel
-- mkReplicate ty dimSl dimOut slix = CUTranslSkel code [] skel
--   where
--     skel = "replicate.inl"
--     code = CTranslUnit
--             ( mkTupleTypeAsc 1 ty ++
--             [ mkDim "Slice"    dimSl
--             , mkDim "SliceDim" dimOut
--             , mkSliceReplicate slix ])
--             (mkNodeInfo (initPos skel) (Name 0))

