{-# LANGUAGE QuasiQuotes #-}

module Data.Array.Accelerate.OpenCL.CodeGen.Util where

import Language.C
import Language.C.Syntax
import Language.C.Quote.C
import Data.Array.Accelerate.OpenCL.CodeGen.Data

import Data.Loc
import Data.Symbol

data Direction = Forward | Backward

-- Common device functions
-- -----------------------

mkIdentity :: Exp -> Func
mkIdentity = mkDeviceFun "identity" (typename "TyOut") []

mkApply :: Int -> Exp -> Func
mkApply argc
  = mkDeviceFun "apply" (typename "TyOut")
  $ params $ map (\n -> (typename ("TyIn"++ show n), 'x':show n)) [argc-1,argc-2..0]

mkProject :: Direction -> Exp -> Func
mkProject Forward  = mkDeviceFun "project" (typename "DimOut") $ params [(typename "DimIn0","x0")]
mkProject Backward = mkDeviceFun "project" (typename "DimIn0") $ params [(typename "DimOut","x0")]

-- mkSliceIndex :: Exp -> Func
-- mkSliceIndex =
--   mkDeviceFun "sliceIndex" (typename "SliceDim") [(typename "Slice","sl"), (typename "CoSlice","co")]

-- mkSliceReplicate :: Exp -> Func
-- mkSliceReplicate =
--   mkDeviceFun "sliceIndex" (typename "Slice") [(typename "SliceDim","dim")]


-- Helper functions
-- ----------------

cvar :: String -> Exp
cvar x = [cexp|$id:x|] --Var (Id x) noSrcLoc

ccall :: String -> [Exp] -> Exp
ccall fn args = [cexp|$id:fn ($args:args)|]

typename :: String -> Type
typename var = Type (DeclSpec [] [] (Tnamed (Id var noSrcLoc) noSrcLoc) noSrcLoc) (DeclRoot noSrcLoc) noSrcLoc

param :: Type -> String -> Param
param ty name = [cparam|$ty:ty $id:name|]

cChar :: Char -> Exp
cChar c = [cexp|$char:c|]

cInteger :: Integer -> Exp
cInteger n = [cexp|$int:n|]

-- TODO investigate why this must be a Rational?
cFloat :: Rational -> Exp
cFloat n = [cexp|$float:n|]

params :: [(Type, String)] -> [Param]
params tynames = map (uncurry param) tynames

fromBool :: Bool -> Exp
fromBool True  = [cexp|1|]
fromBool False = [cexp|0|]

-- mkDim :: String -> Int -> CExtDecl
-- mkDim name n =
--   mkTypedef name False False [CTypeDef (internalIdent ("DIM" ++ show n)) noSrcLoc]

-- mkTypedef :: String -> Bool -> Bool -> CType -> CExtDecl
-- mkTypedef var volatile ptr ty =
--   CDeclExt $ CDecl
--     (CStorageSpec (CTypedef noSrcLoc) : [TypeQual (CVolatQual noSrcLoc) | volatile] ++ map CTypeSpec ty)
--     [(Just (CDeclr (Just (internalIdent var)) [CPtrDeclr [] noSrcLoc | ptr] Nothing [] noSrcLoc), Nothing, Nothing)]
--     noSrcLoc

-- mkShape :: Int -> String -> CExtDecl
-- mkShape d n = mkGlobal [constant,dimension] n
--   where
--     constant  = TypeQual (CAttrQual (CAttr (internalIdent "constant") [] noSrcLoc))
--     dimension = CTypeSpec (CTypeDef (internalIdent ("DIM" ++ show d)) noSrcLoc)

-- mkGlobal :: [CDeclSpec] -> String -> CExtDecl
-- mkGlobal spec name =
--   CDeclExt (CDecl (CStorageSpec (CStatic noSrcLoc) : spec)
--            [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] noSrcLoc),Nothing,Nothing)] noSrcLoc)

-- mkInitList :: [Exp] -> CInit
-- mkInitList []  = CInitExpr (CConst (CIntConst (cInteger 0) noSrcLoc)) noSrcLoc
-- mkInitList [x] = CInitExpr x noSrcLoc
-- mkInitList xs  = CInitList (map (\e -> ([],CInitExpr e noSrcLoc)) xs) noSrcLoc


-- -- typedef struct {
-- --   ... (volatile?) ty1 (*?) a1; (volatile?) ty0 (*?) a0;
-- -- } var;
-- --
-- -- NOTE: The Accelerate language uses snoc based tuple projection, so the last
-- --       field of the structure is named 'a' instead of the first.
-- --
-- mkStruct :: String -> Bool -> Bool -> [CType] -> CExtDecl
-- mkStruct name volatile ptr types =
--   CDeclExt $ CDecl
--     [CStorageSpec (CTypedef noSrcLoc) , CTypeSpec (CSUType (CStruct CStructTag Nothing (Just (zipWith field names types)) [] noSrcLoc) noSrcLoc)]
--     [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] noSrcLoc),Nothing,Nothing)]
--     noSrcLoc
--   where
--     names      = reverse . take (length types) $ (enumFrom 0 :: [Int])
--     field v ty = CDecl ([TypeQual (CVolatQual noSrcLoc) | volatile] ++ map CTypeSpec ty)
--                        [(Just (CDeclr (Just (internalIdent ('a':show v))) [CPtrDeclr [] noSrcLoc | ptr] Nothing [] noSrcLoc), Nothing, Nothing)]
--                        noSrcLoc


-- -- typedef struct __attribute__((aligned(n * sizeof(ty)))) {
-- --     ty [x, y, z, w];
-- -- } var;
-- --
-- mkTyVector :: String -> Int -> CType -> CExtDecl
-- mkTyVector var n ty =
--   CDeclExt $ CDecl
--     [CStorageSpec (CTypedef noSrcLoc), CTypeSpec (CSUType (CStruct CStructTag Nothing (Just [CDecl (map CTypeSpec ty) fields noSrcLoc]) [CAttr (internalIdent "aligned") [CBinary CMulOp (CConst (CIntConst (cInteger (toInteger n)) noSrcLoc)) (CSizeofType (CDecl (map CTypeSpec ty) [] noSrcLoc) noSrcLoc) noSrcLoc] noSrcLoc] noSrcLoc) noSrcLoc)]
--     [(Just (CDeclr (Just (internalIdent var)) [] Nothing [] noSrcLoc), Nothing, Nothing)]
--     noSrcLoc
--   where
--     fields = take n . flip map "xyzw" $ \f ->
--       (Just (CDeclr (Just (internalIdent [f])) [] Nothing [] noSrcLoc), Nothing, Nothing)


mkDeviceFun :: String -> Type -> [Param] -> Exp -> Func
mkDeviceFun name tyout args expr =
  [cfun| static inline $ty:tyout $id:name($params:args)
          {
            $ty:tyout r = { $exp:expr };
            return r;
          }
  |]

mkDeviceFun' :: String -> Type -> [Param] -> [Stm] -> Func
mkDeviceFun' name tyout args body =
  [cfun| static inline $ty:tyout $id:name($params:args)
          {
            $stms:body
          }
  |]
