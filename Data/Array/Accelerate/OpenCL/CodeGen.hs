{-# LANGUAGE CPP, GADTs, PatternGuards, ScopedTypeVariables, TemplateHaskell, QuasiQuotes #-}
-- |
-- Module      : Data.Array.Accelerate.OpenCL.CodeGen
-- Copyright   : [2011] Martin Dybdal
-- License     : BSD3
--
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.OpenCL.CodeGen (

  -- * types
  CUTranslSkel, AccBinding(..),

  -- * code generation
  codeGenAcc, codeGenFun, codeGenExp

) where

import Data.Char

import qualified Language.C as C
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax
import qualified Data.Loc as Loc
import Language.C.Quote.C

import Text.PrettyPrint

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Pretty ()
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Analysis.Shape
--import Data.Array.Accelerate.Analysis.Stencil
--import Data.Array.Accelerate.Array.Representation
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar
import qualified Foreign.Storable                               as F

import Data.Array.Accelerate.OpenCL.CodeGen.Data
import Data.Array.Accelerate.OpenCL.CodeGen.Util
import Data.Array.Accelerate.OpenCL.CodeGen.Skeleton


#include "accelerate.h"


-- Array computations that were embedded within scalar expressions, and will be
-- required to execute the kernel; i.e. bound to texture references or similar.
--
data AccBinding aenv where
  ArrayVar :: (Sugar.Shape sh, Sugar.Elt e)
           => Idx aenv (Sugar.Array sh e) -> AccBinding aenv

instance Eq (AccBinding aenv) where
  ArrayVar ix1 == ArrayVar ix2 = idxToInt ix1 == idxToInt ix2



-- -- Array expressions
-- -- -----------------

-- | Instantiate an array computation with a set of concrete function and type
-- definitions to fix the parameters of an algorithmic skeleton. The generated
-- code can then be pretty-printed to file, and compiled to object code
-- executable on the device.
--
-- The code generator needs to include binding points for array references from
-- scalar code. We require that the only array form allowed within expressions
-- are array variables.
--
codeGenAcc :: forall aenv a. OpenAcc aenv a -> [AccBinding aenv] -> CUTranslSkel
codeGenAcc acc vars =
  let --fvars                      = concatMap (liftAcc acc) vars
      CUTranslSkel code = codeGen acc
--      CTranslUnit decl node      = code
  in
   --CUTranslSkel (CTranslUnit (fvars ++ decl) node) def skel
   CUTranslSkel code
  where
    codeGen :: OpenAcc aenv a -> CUTranslSkel
    codeGen (OpenAcc pacc) =
      case pacc of
        -- non-computation forms
        --
        Let _ _           -> internalError
        Let2 _ _          -> internalError
        Avar _            -> internalError
        Apply _ _         -> internalError
        Acond _ _ _       -> internalError
        PairArrays _ _    -> internalError
        Use _             -> internalError
        Unit _            -> internalError
        Reshape _ _       -> internalError

        -- computation nodes
        --
        Generate _ f      -> mkGenerate (codeGenAccTypeDim acc) (codeGenFun f)
        -- Fold f e a        -> mkFold  (codeGenAccTypeDim a) (codeGenExp e) (codeGenFun f)
        -- Fold1 f a         -> mkFold1 (codeGenAccTypeDim a) (codeGenFun f)
        -- FoldSeg f e a s   -> mkFoldSeg  (codeGenAccTypeDim a) (codeGenAccType s) (codeGenExp e) (codeGenFun f)
        -- Fold1Seg f a s    -> mkFold1Seg (codeGenAccTypeDim a) (codeGenAccType s) (codeGenFun f)
        -- Scanl f e _       -> mkScanl  (codeGenExpType e) (codeGenExp e) (codeGenFun f)
        -- Scanr f e _       -> mkScanr  (codeGenExpType e) (codeGenExp e) (codeGenFun f)
        -- Scanl' f e _      -> mkScanl' (codeGenExpType e) (codeGenExp e) (codeGenFun f)
        -- Scanr' f e _      -> mkScanr' (codeGenExpType e) (codeGenExp e) (codeGenFun f)
        -- Scanl1 f a        -> mkScanl1 (codeGenAccType a) (codeGenFun f)
        -- Scanr1 f a        -> mkScanr1 (codeGenAccType a) (codeGenFun f)
        Map f a           -> mkMap (codeGenAccType acc) (codeGenAccType a) (codeGenFun f)
        ZipWith f a b     -> mkZipWith (codeGenAccTypeDim acc) (codeGenAccTypeDim a) (codeGenAccTypeDim b) (codeGenFun f)
        Permute f _ g a   -> mkPermute (codeGenAccType a) (accDim acc) (accDim a) (codeGenFun f) (codeGenFun g)
        Backpermute _ f a -> mkBackpermute (codeGenAccType a) (accDim acc) (accDim a) (codeGenFun f)
        -- Replicate sl _ a  ->
        --   let dimSl  = accDim a
        --       dimOut = accDim acc
        --       --
        --       extend :: SliceIndex slix sl co dim -> Int -> [CExpr]
        --       extend (SliceNil)            _ = []
        --       extend (SliceAll   sliceIdx) n = mkPrj dimOut "dim" n : extend sliceIdx (n+1)
        --       extend (SliceFixed sliceIdx) n = extend sliceIdx (n+1)
        --   in
        --   mkReplicate (codeGenAccType a) dimSl dimOut . reverse $ extend sl 0

--         -- Index sl a slix   ->
--         --   let dimCo  = length (codeGenExpType slix)
--         --       dimSl  = accDim acc
--         --       dimIn0 = accDim a
--         --       --
--         --       restrict :: SliceIndex slix sl co dim -> (Int,Int) -> [CExpr]
--         --       restrict (SliceNil)            _     = []
--         --       restrict (SliceAll   sliceIdx) (m,n) = mkPrj dimSl "sl" n : restrict sliceIdx (m,n+1)
--         --       restrict (SliceFixed sliceIdx) (m,n) = mkPrj dimCo "co" m : restrict sliceIdx (m+1,n)
--         --   in
--         --   mkIndex (codeGenAccType a) dimSl dimCo dimIn0 . reverse $ restrict sl (0,0)

--         -- Stencil f bndy a     ->
--         --   let ty0   = codeGenTupleTex (accType a)
--         --       decl0 = map (map CTypeSpec) (reverse ty0)
--         --       sten0 = zipWith mkGlobal decl0 (map (\n -> "stencil0_a" ++ show n) [0::Int ..])
--         --   in
--         --   mkStencil (codeGenAccTypeDim acc)
--         --             sten0 (codeGenAccType a) (map (reverse . Sugar.shapeToList) $ offsets f a) (codeGenBoundary a bndy)
--         --             (codeGenFun f)

--         -- Stencil2 f bndy1 a1 bndy0 a0 ->
--         --   let ty1          = codeGenTupleTex (accType a1)
--         --       ty0          = codeGenTupleTex (accType a0)
--         --       decl         = map (map CTypeSpec) . reverse
--         --       sten n       = zipWith (flip mkGlobal) (map (\k -> "stencil" ++ shows (n::Int) "_a" ++ show k) [0::Int ..]) . decl
--         --       (pos1, pos0) = offsets2 f a1 a0
--         --   in
--         --   mkStencil2 (codeGenAccTypeDim acc)
--         --              (sten 1 ty1) (codeGenAccType a1) (map (reverse . Sugar.shapeToList) pos1) (codeGenBoundary a1 bndy1)
--         --              (sten 0 ty0) (codeGenAccType a0) (map (reverse . Sugar.shapeToList) pos0) (codeGenBoundary a0 bndy0)
--         --              (codeGenFun f)

    -- --
    -- -- Generate binding points (texture references and shapes) for arrays lifted
    -- -- from scalar expressions
    -- --
    -- liftAcc :: OpenAcc aenv a -> AccBinding aenv -> [CExtDecl]
    -- liftAcc _ (ArrayVar idx) =
    --   let avar    = OpenAcc (Avar idx)
    --       idx'    = show $ idxToInt idx
    --       sh      = mkShape (accDim avar) ("sh" ++ idx')
    --       ty      = codeGenTupleTex (accType avar)
    --       arr n   = "arr" ++ idx' ++ "_a" ++ show n
    --       var t n = mkGlobal (map CTypeSpec t) (arr n)
    --   in
    --   sh : zipWith var (reverse ty) (enumFrom 0 :: [Int])

    --
    -- caffeine and misery
    --
    internalError =
      let msg = unlines ["unsupported array primitive", render (nest 2 doc)]
          ppr = show acc
          doc | length ppr <= 250 = text ppr
              | otherwise         = text (take 250 ppr) <+> text "... {truncated}"
      in
      INTERNAL_ERROR(error) "codeGenAcc" msg


-- -- code generation for stencil boundary conditions
-- --
-- codeGenBoundary :: forall aenv dim e. Sugar.Elt e
--                 => OpenAcc aenv (Sugar.Array dim e) {- dummy -}
--                 -> Boundary (Sugar.EltRepr e)
--                 -> Boundary [CExpr]
-- codeGenBoundary _ (Constant c) = Constant $ codeGenConst (Sugar.eltType (undefined::e)) c
-- codeGenBoundary _ Clamp        = Clamp
-- codeGenBoundary _ Mirror       = Mirror
-- codeGenBoundary _ Wrap         = Wrap


mkPrj :: Int -> String -> Int -> C.Exp
mkPrj ndim var c
 | ndim <= 1 = cvar var
 | otherwise = [cexp| $exp:v . $id:field |] --CMember (cvar var) (internalIdent ('a':show c)) False internalNode
                   where v = cvar var
                         field = 'a' : show c

-- Scalar Expressions
-- ------------------

-- Function abstraction
--
-- Although Accelerate includes lambda abstractions, it does not include a
-- general application form. That is, lambda abstractions of scalar expressions
-- are only introduced as arguments to collective operations, so lambdas are
-- always outermost, and can always be translated into plain C functions.
--
codeGenFun :: OpenFun env aenv t -> C.Exp
codeGenFun (Lam  lam)  = codeGenFun lam
codeGenFun (Body body) = seqexps $ codeGenExp body

seqexps :: [C.Exp] -> C.Exp
seqexps = foldl1 seqe

seqe :: C.Exp -> C.Exp -> C.Exp
e1 `seqe` e2 = C.Seq e1 e2 Loc.noSrcLoc


-- Embedded scalar computations
--
-- The state is used here to track array expressions that have been hoisted out
-- of the scalar computation; namely, the arguments to 'IndexScalar' and 'Shape'
--
codeGenExp :: forall env aenv t. OpenExp env aenv t -> [C.Exp]
codeGenExp (PrimConst c)   = [codeGenPrimConst c]
codeGenExp (PrimApp f arg) = [codeGenPrim f (codeGenExp arg)]
codeGenExp (Const c)       = codeGenConst (Sugar.eltType (undefined::t)) c
codeGenExp (Tuple t)       = codeGenTup t
codeGenExp p@(Prj idx e)
  = reverse
  . take (length $ codeGenTupleType (expType p))
  . drop (prjToInt idx (expType e))
  . reverse
  $ codeGenExp e

codeGenExp IndexNil         = []
codeGenExp IndexAny         = INTERNAL_ERROR(error) "codeGenExp" "IndexAny: not implemented yet"
codeGenExp (IndexCons ix i) = codeGenExp ix ++ codeGenExp i

codeGenExp (IndexHead sh@(Shape a)) =
  let [var] = codeGenExp sh
  in if accDim a > 1
        then [ [cexp|$exp:var . a0|] ]
        else [var]

codeGenExp (IndexTail sh@(Shape a)) =
  let [var] = codeGenExp sh
      idx   = reverse [1 .. accDim a - 1]
  in
  flip map idx (\i -> let field = 'a' : show i
                      in [cexp|$exp:var . $id:field|])


codeGenExp (IndexHead ix) = return . last $ codeGenExp ix
codeGenExp (IndexTail ix) =          init $ codeGenExp ix

codeGenExp (Var i) =
  let var = cvar ('x' : idxToString i)
  in
  case codeGenTupleType (Sugar.eltType (undefined::t)) of
       [_] -> [var]
       cps -> reverse . take (length cps) . flip map (enumFrom 0 :: [Int]) $
         \c -> let field = 'a':show c
               in [cexp|$exp:var . $id:field |]

codeGenExp (Cond p t e) =
  let [predicate] = codeGenExp p
      branch a b  = [cexp| $exp:predicate ? $exp:a : $exp:b |]
  in
  zipWith branch (codeGenExp t) (codeGenExp e)

codeGenExp (Size a)         = return $ ccall "size" (codeGenExp (Shape a))
codeGenExp (Shape a)
  | OpenAcc (Avar var) <- a = return $ cvar ("sh" ++ show (idxToInt var))
  | otherwise               = INTERNAL_ERROR(error) "codeGenExp" "expected array variable"

idxToString :: forall env t. Idx env t -> String
idxToString idx = [chr (ord 'A' + idxToInt idx)]

-- codeGenExp (IndexScalar a e)
--   | OpenAcc (Avar var) <- a =
--       let var'  = show $ idxToInt var
--           arr n = cvar ("arr" ++ var' ++ "_a" ++ show n)
--           sh    = cvar ("sh"  ++ var')
--           ix    = ccall "toIndex" [sh, ccall "shape" (codeGenExp e)]
--           --
--           ty         = codeGenTupleTex (accType a)
--           indexA t n = ccall indexer [arr n, ix]
--             where
--               indexer = case t of
--                           [CDoubleType _] -> "indexDArray"
--                           _               -> "indexArray"
--       in
--       reverse $ zipWith indexA (reverse ty) (enumFrom 0 :: [Int])
--   | otherwise               = INTERNAL_ERROR(error) "codeGenExp" "expected array variable"


-- Tuples are defined as snoc-lists, so generate code right-to-left
--
codeGenTup :: Tuple (OpenExp env aenv) t -> [C.Exp]
codeGenTup NilTup          = []
codeGenTup (t `SnocTup` e) = codeGenTup t ++ codeGenExp e

-- Convert a tuple index into the corresponding integer. Since the internal
-- representation is flat, be sure to walk over all sub components when indexing
-- past nested tuples.
--
prjToInt :: TupleIdx t e -> TupleType a -> Int
prjToInt ZeroTupIdx     _                 = 0
prjToInt (SuccTupIdx i) (b `PairTuple` a) = length (codeGenTupleType a) + prjToInt i b
prjToInt _ _ =
  INTERNAL_ERROR(error) "prjToInt" "inconsistent valuation"


-- Types
-- -----

-- Generate types for the reified elements of an array computation
--
codeGenAccType :: OpenAcc aenv (Sugar.Array dim e) -> [C.Type]
codeGenAccType =  codeGenTupleType . accType

codeGenExpType :: OpenExp aenv env t -> [C.Type]
codeGenExpType =  codeGenTupleType . expType

codeGenAccTypeDim :: OpenAcc aenv (Sugar.Array dim e) -> ([C.Type],Int)
codeGenAccTypeDim acc = (codeGenAccType acc, accDim acc)


-- Implementation
--
codeGenTupleType :: TupleType a -> [C.Type]
codeGenTupleType UnitTuple         = []
codeGenTupleType (SingleTuple  ty) = [codeGenScalarType ty]
codeGenTupleType (PairTuple t1 t0) = codeGenTupleType t1 ++ codeGenTupleType t0

codeGenScalarType :: ScalarType a -> C.Type
codeGenScalarType (NumScalarType    ty) = codeGenNumType ty
codeGenScalarType (NonNumScalarType ty) = codeGenNonNumType ty

codeGenNumType :: NumType a -> C.Type
codeGenNumType (IntegralNumType ty) = codeGenIntegralType ty
codeGenNumType (FloatingNumType ty) = codeGenFloatingType ty

codeGenIntegralType :: IntegralType a -> C.Type
codeGenIntegralType (TypeInt8    _) = [cty|char|]
codeGenIntegralType (TypeInt16   _) = [cty|short|]
codeGenIntegralType (TypeInt32   _) = [cty|int|]
codeGenIntegralType (TypeInt64   _) = [cty|long|]
codeGenIntegralType (TypeWord8   _) = [cty|unsigned char|]
codeGenIntegralType (TypeWord16  _) = [cty|unsigned short|]
codeGenIntegralType (TypeWord32  _) = [cty|unsigned int|]
codeGenIntegralType (TypeWord64  _) = [cty|unsigned long|]
codeGenIntegralType (TypeCShort  _) = [cty|short|]
codeGenIntegralType (TypeCUShort _) = [cty|unsigned short|]
codeGenIntegralType (TypeCInt    _) = [cty|int|]
codeGenIntegralType (TypeCUInt   _) = [cty|unsigned int|]
codeGenIntegralType (TypeCLong   _) = [cty|long int|]
codeGenIntegralType (TypeCULong  _) = [cty|unsigned long int|]
codeGenIntegralType (TypeCLLong  _) = [cty|long long int|]
codeGenIntegralType (TypeCULLong _) = [cty|unsigned long long int|]

codeGenIntegralType (TypeInt     _) =
  case F.sizeOf (undefined::Int) of
       4 -> [cty|int|]
       8 -> [cty|long int|]
       _ -> error "we can never get here"

codeGenIntegralType (TypeWord    _) =
  case F.sizeOf (undefined::Int) of
       4 -> [cty|unsigned int|]
       8 -> [cty|unsigned long int|]
       _ -> error "we can never get here"

codeGenFloatingType :: FloatingType a -> C.Type
codeGenFloatingType (TypeFloat   _) = [cty|float|]
codeGenFloatingType (TypeDouble  _) = [cty|double|]
codeGenFloatingType (TypeCFloat  _) = [cty|float|]
codeGenFloatingType (TypeCDouble _) = [cty|double|]

codeGenNonNumType :: NonNumType a -> C.Type
codeGenNonNumType (TypeBool   _) = error "codeGenNonNum :: Bool" -- [CUnsigType internalNode, CCharType internalNode]
codeGenNonNumType (TypeChar   _) = error "codeGenNonNum :: Char" -- [CCharType internalNode]
codeGenNonNumType (TypeCChar  _) = [cty|char|]
codeGenNonNumType (TypeCSChar _) = [cty|signed char|]
codeGenNonNumType (TypeCUChar _) = [cty|unsigned char|]

-- Scalar Primitives
-- -----------------

codeGenPrimConst :: PrimConst a -> C.Exp
codeGenPrimConst (PrimMinBound ty) = codeGenMinBound ty
codeGenPrimConst (PrimMaxBound ty) = codeGenMaxBound ty
codeGenPrimConst (PrimPi       ty) = codeGenPi ty

codeGenPrim :: PrimFun p -> [C.Exp] -> C.Exp
codeGenPrim (PrimAdd              _) [a,b] = [cexp|$exp:a + $exp:b|]
codeGenPrim (PrimSub              _) [a,b] = [cexp|$exp:a - $exp:b|]
codeGenPrim (PrimMul              _) [a,b] = [cexp|$exp:a * $exp:b|]
codeGenPrim (PrimNeg              _) [a]   = [cexp| - $exp:a|]
codeGenPrim (PrimAbs             ty) [a]   = codeGenAbs ty a
codeGenPrim (PrimSig             ty) [a]   = codeGenSig ty a
codeGenPrim (PrimQuot             _) [a,b] = [cexp|$exp:a / $exp:b|]
codeGenPrim (PrimRem              _) [a,b] = [cexp|$exp:a % $exp:b|]
codeGenPrim (PrimIDiv             _) [a,b] = ccall "idiv" [a,b]
codeGenPrim (PrimMod              _) [a,b] = ccall "mod"  [a,b]
codeGenPrim (PrimBAnd             _) [a,b] = [cexp|$exp:a & $exp:b|]
codeGenPrim (PrimBOr              _) [a,b] = [cexp|$exp:a | $exp:b|]
codeGenPrim (PrimBXor             _) [a,b] = [cexp|$exp:a ^ $exp:b|]
codeGenPrim (PrimBNot             _) [a]   = [cexp|~ $exp:a|]
codeGenPrim (PrimBShiftL          _) [a,b] = [cexp|$exp:a << $exp:b|]
codeGenPrim (PrimBShiftR          _) [a,b] = [cexp|$exp:a >> $exp:b|]
codeGenPrim (PrimBRotateL         _) [a,b] = ccall "rotateL" [a,b]
codeGenPrim (PrimBRotateR         _) [a,b] = ccall "rotateR" [a,b]
codeGenPrim (PrimFDiv             _) [a,b] = [cexp|$exp:a / $exp:b|]
codeGenPrim (PrimRecip           ty) [a]   = codeGenRecip ty a
codeGenPrim (PrimSin             ty) [a]   = ccall (FloatingNumType ty `postfix` "sin")   [a]
codeGenPrim (PrimCos             ty) [a]   = ccall (FloatingNumType ty `postfix` "cos")   [a]
codeGenPrim (PrimTan             ty) [a]   = ccall (FloatingNumType ty `postfix` "tan")   [a]
codeGenPrim (PrimAsin            ty) [a]   = ccall (FloatingNumType ty `postfix` "asin")  [a]
codeGenPrim (PrimAcos            ty) [a]   = ccall (FloatingNumType ty `postfix` "acos")  [a]
codeGenPrim (PrimAtan            ty) [a]   = ccall (FloatingNumType ty `postfix` "atan")  [a]
codeGenPrim (PrimAsinh           ty) [a]   = ccall (FloatingNumType ty `postfix` "asinh") [a]
codeGenPrim (PrimAcosh           ty) [a]   = ccall (FloatingNumType ty `postfix` "acosh") [a]
codeGenPrim (PrimAtanh           ty) [a]   = ccall (FloatingNumType ty `postfix` "atanh") [a]
codeGenPrim (PrimExpFloating     ty) [a]   = ccall (FloatingNumType ty `postfix` "exp")   [a]
codeGenPrim (PrimSqrt            ty) [a]   = ccall (FloatingNumType ty `postfix` "sqrt")  [a]
codeGenPrim (PrimLog             ty) [a]   = ccall (FloatingNumType ty `postfix` "log")   [a]
codeGenPrim (PrimFPow            ty) [a,b] = ccall (FloatingNumType ty `postfix` "pow")   [a,b]
codeGenPrim (PrimLogBase         ty) [a,b] = codeGenLogBase ty a b
codeGenPrim (PrimTruncate     ta tb) [a]   = codeGenTruncate ta tb a
codeGenPrim (PrimRound        ta tb) [a]   = codeGenRound ta tb a
codeGenPrim (PrimFloor        ta tb) [a]   = codeGenFloor ta tb a
codeGenPrim (PrimCeiling      ta tb) [a]   = codeGenCeiling ta tb a
codeGenPrim (PrimAtan2           ty) [a,b] = ccall (FloatingNumType ty `postfix` "atan2") [a,b]
codeGenPrim (PrimLt               _) [a,b] = [cexp|$exp:a < $exp:b|]
codeGenPrim (PrimGt               _) [a,b] = [cexp|$exp:a > $exp:b|]
codeGenPrim (PrimLtEq             _) [a,b] = [cexp|$exp:a <= $exp:b|]
codeGenPrim (PrimGtEq             _) [a,b] = [cexp|$exp:a >= $exp:b|]
codeGenPrim (PrimEq               _) [a,b] = [cexp|$exp:a == $exp:b|]
codeGenPrim (PrimNEq              _) [a,b] = [cexp|$exp:a != $exp:b|]
codeGenPrim (PrimMax             ty) [a,b] = codeGenMax ty a b
codeGenPrim (PrimMin             ty) [a,b] = codeGenMin ty a b
codeGenPrim PrimLAnd                 [a,b] = [cexp|$exp:a && $exp:b|]
codeGenPrim PrimLOr                  [a,b] = [cexp|$exp:a || $exp:b|]
codeGenPrim PrimLNot                 [a]   = [cexp| ! $exp:a|]
codeGenPrim PrimOrd                  [a]   = codeGenOrd a
codeGenPrim PrimChr                  [a]   = codeGenChr a
codeGenPrim PrimBoolToInt            [a]   = codeGenBoolToInt a
codeGenPrim (PrimFromIntegral ta tb) [a]   = codeGenFromIntegral ta tb a

-- If the argument lists are not the correct length
codeGenPrim _ _ =
  INTERNAL_ERROR(error) "codeGenPrim" "inconsistent valuation"


-- Implementation of scalar primitives
--
codeGenConst :: TupleType a -> a -> [C.Exp]
codeGenConst UnitTuple           _      = []
codeGenConst (SingleTuple ty)    c      = [codeGenScalar ty c]
codeGenConst (PairTuple ty1 ty0) (cs,c) = codeGenConst ty1 cs ++ codeGenConst ty0 c

-- Scalar constants
--
-- Add an explicit type annotation (cast) to all scalar constants, which avoids
-- ambiguity as to what type we actually want. Without this:
--
--   1. Floating-point constants will be implicitly promoted to double
--      precision, which will emit warnings on pre-1.3 series devices and
--      unnecessary runtime conversion and register pressure on later hardware
--      that actually does support double precision arithmetic.
--
--   2. Interaction of differing word sizes on the host and device in overloaded
--      functions such as max() leads to ambiguity.
--
codeGenScalar :: ScalarType a -> a -> C.Exp
codeGenScalar st c = ccast st $ case st of
  NumScalarType (IntegralNumType ty)
    | IntegralDict <- integralDict ty -> cInteger $ fromIntegral c
  NumScalarType (FloatingNumType ty)
    | FloatingDict <- floatingDict ty -> cFloat $ realToFrac c
  NonNumScalarType (TypeCChar  _)     -> cChar . chr . fromIntegral $ c
  NonNumScalarType (TypeCUChar _)     -> cChar . chr . fromIntegral $ c
  NonNumScalarType (TypeCSChar _)     -> cChar . chr . fromIntegral $ c
  NonNumScalarType (TypeChar   _)     -> cChar c
  NonNumScalarType (TypeBool   _)     -> fromBool c

-- Constant methods of floating

codeGenPi :: FloatingType a -> C.Exp
codeGenPi ty
  | FloatingDict <- floatingDict ty
  = codeGenScalar (NumScalarType (FloatingNumType ty)) pi

-- Constant methods of bounded

codeGenMinBound :: BoundedType a -> C.Exp
codeGenMinBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty
  = codeGenScalar (NumScalarType (IntegralNumType ty)) minBound
codeGenMinBound (NonNumBoundedType   ty)
  | NonNumDict   <- nonNumDict   ty
  = codeGenScalar (NonNumScalarType ty) minBound

codeGenMaxBound :: BoundedType a -> C.Exp
codeGenMaxBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty
  = codeGenScalar (NumScalarType (IntegralNumType ty)) maxBound
codeGenMaxBound (NonNumBoundedType   ty)
  | NonNumDict   <- nonNumDict   ty
  = codeGenScalar (NonNumScalarType ty) maxBound

-- Methods from Num, Floating, Fractional and RealFrac

codeGenAbs :: NumType a -> C.Exp -> C.Exp
codeGenAbs ty@(IntegralNumType _) x = ccall (ty `postfix` "abs")  [x]
codeGenAbs ty@(FloatingNumType _) x = ccall (ty `postfix` "fabs") [x]

-- TODO investigate whether codeGenScalar should be necessary
codeGenSig :: NumType a -> C.Exp -> C.Exp
codeGenSig ty@(IntegralNumType t) a
  | IntegralDict <- integralDict t
  = [cexp| $exp:a >= 0 ? 1 : 0|]
    -- where one = codeGenScalar (NumScalarType ty) 1
    --       zero = codeGenScalar (NumScalarType ty) 0
codeGenSig ty@(FloatingNumType t) a
  | FloatingDict <- floatingDict t
  = [cexp| $exp:a >= 0 ? 1 : 0|]
    -- where one = codeGenScalar (NumScalarType ty) 1
    --       zero = codeGenScalar (NumScalarType ty) 0

codeGenRecip :: FloatingType a -> C.Exp -> C.Exp
codeGenRecip ty x | FloatingDict <- floatingDict ty
  = let a = (codeGenScalar (NumScalarType (FloatingNumType ty)) 1)
    in [cexp| $exp:a / $exp:x |]

codeGenLogBase :: FloatingType a -> C.Exp -> C.Exp -> C.Exp
codeGenLogBase ty x y = let a = ccall (FloatingNumType ty `postfix` "log") [x]
                            b = ccall (FloatingNumType ty `postfix` "log") [y]
                        in [cexp| $exp:b / $exp:a |]

codeGenMin :: ScalarType a -> C.Exp -> C.Exp -> C.Exp
codeGenMin (NumScalarType ty@(IntegralNumType _)) a b = ccall (ty `postfix` "min")  [a,b]
codeGenMin (NumScalarType ty@(FloatingNumType _)) a b = ccall (ty `postfix` "fmin") [a,b]
codeGenMin (NonNumScalarType _)                   a b =
  let ty = NumScalarType (IntegralNumType (TypeInt32 (undefined :: IntegralDict Int32)))
  in  codeGenMin ty (ccast ty a) (ccast ty b)

codeGenMax :: ScalarType a -> C.Exp -> C.Exp -> C.Exp
codeGenMax (NumScalarType ty@(IntegralNumType _)) a b = ccall (ty `postfix` "max")  [a,b]
codeGenMax (NumScalarType ty@(FloatingNumType _)) a b = ccall (ty `postfix` "fmax") [a,b]
codeGenMax (NonNumScalarType _)                   a b =
  let ty = NumScalarType (IntegralNumType (TypeInt32 (undefined :: IntegralDict Int32)))
  in  codeGenMax ty (ccast ty a) (ccast ty b)


-- Type coercions

codeGenOrd :: C.Exp -> C.Exp
codeGenOrd = ccast (NumScalarType (IntegralNumType (TypeInt (undefined :: IntegralDict Int))))

codeGenChr :: C.Exp -> C.Exp
codeGenChr = ccast (NonNumScalarType (TypeChar (undefined :: NonNumDict Char)))

codeGenBoolToInt :: C.Exp -> C.Exp
codeGenBoolToInt = ccast (NumScalarType (IntegralNumType (TypeInt (undefined :: IntegralDict Int))))

codeGenFromIntegral :: IntegralType a -> NumType b -> C.Exp -> C.Exp
codeGenFromIntegral _ ty = ccast (NumScalarType ty)

codeGenTruncate :: FloatingType a -> IntegralType b -> C.Exp -> C.Exp
codeGenTruncate ta tb x
  = ccast (NumScalarType (IntegralNumType tb))
  $ ccall (FloatingNumType ta `postfix` "trunc") [x]

codeGenRound :: FloatingType a -> IntegralType b -> C.Exp -> C.Exp
codeGenRound ta tb x
  = ccast (NumScalarType (IntegralNumType tb))
  $ ccall (FloatingNumType ta `postfix` "round") [x]

codeGenFloor :: FloatingType a -> IntegralType b -> C.Exp -> C.Exp
codeGenFloor ta tb x
  = ccast (NumScalarType (IntegralNumType tb))
  $ ccall (FloatingNumType ta `postfix` "floor") [x]

codeGenCeiling :: FloatingType a -> IntegralType b -> C.Exp -> C.Exp
codeGenCeiling ta tb x
  = ccast (NumScalarType (IntegralNumType tb))
  $ ccall (FloatingNumType ta `postfix` "ceil") [x]


-- Auxiliary Functions
-- -------------------

ccast :: ScalarType a -> C.Exp -> C.Exp
ccast ty x = [cexp|($ty:t) $exp:x|]
  where t = codeGenScalarType ty

postfix :: NumType a -> String -> String
postfix (FloatingNumType (TypeFloat  _)) = (++ "f")
postfix (FloatingNumType (TypeCFloat _)) = (++ "f")
postfix _                                = id

