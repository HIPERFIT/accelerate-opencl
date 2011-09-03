{-# LANGUAGE QuasiQuotes #-}

module Data.Array.Accelerate.OpenCL.CodeGen.Util where

import Language.C hiding (mkPtr)
import Language.C.Syntax
import Language.C.Quote.OpenCL

import Data.Loc
import Data.Symbol

import Data.Array.Accelerate.OpenCL.CodeGen.Monad

data Direction = Forward | Backward

-- Types
ix :: Type
ix = typename "Ix"

outType :: Type
outType = typename "TyOut"



-- Common device functions
-- -----------------------

mkIdentity :: Exp -> Definition
mkIdentity = mkDeviceFun "identity" (typename "TyOut") []

mkApply :: Int -> Exp -> CGM ()
mkApply argc exp
  = addDefinition $
      (mkDeviceFun "apply" outType
       $ params $ map (\c -> (typename ("TyIn"++ [c]), 'x' : [c])) $ reverse $ take argc ['A'..]) exp

mkProject :: Direction -> Exp -> CGM ()
mkProject Forward exp =
  addDefinition $
    (mkDeviceFun "project" (typename "DimOut") $ params [(typename "DimInA","xA")]) exp
mkProject Backward exp =
  addDefinition $
    (mkDeviceFun "project" (typename "DimInA") $ params [(typename "DimOut","xA")]) exp

mkSliceIndex :: Exp -> CGM ()
mkSliceIndex exp =
  addDefinition $
    (mkDeviceFun "sliceIndex" (typename "SliceDim") $ params [(typename "Slice","sl"), (typename "CoSlice","co")]) exp

mkSliceReplicate :: Exp -> CGM ()
mkSliceReplicate exp =
  addDefinition $
    (mkDeviceFun "sliceIndex" (typename "Slice") $ [param (typename "SliceDim") "dim"]) exp


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

mkDim :: String -> Int -> CGM ()
mkDim name n = addDefinition [cedecl|typedef $ty:dim $id:name;|]
   where dim = typename $ "DIM" ++ show n

mkVolatile :: Type -> Type
mkVolatile (Type (DeclSpec storage quals typ l0) _ l1) =
  Type (DeclSpec storage ((Tvolatile noSrcLoc) : quals) typ l0) (DeclRoot noSrcLoc) l1
mkVolatile _ = error "Not a DeclSpec"

mkPtr :: Type -> Type
mkPtr (Type (DeclSpec storage quals typ l0) _ l1) =
  Type (DeclSpec storage quals typ l0) (Ptr [] (DeclRoot noSrcLoc) noSrcLoc) l1
mkPtr _ = error "Not a DeclSpec"

mkGlobal :: Type -> Type
mkGlobal (Type (DeclSpec storage quals typ l0) _ l1) =
  Type (DeclSpec storage ((TCLGlobal noSrcLoc) : quals) typ l0) (DeclRoot noSrcLoc) l1
mkGlobal _ = error "Not a DeclSpec"


mkTypedef :: Bool -> String -> Type -> Definition
mkTypedef volatile tyname typ | volatile = let typ' = mkVolatile typ
                                           in [cedecl|typedef $ty:typ' $id:tyname;|]
                              | otherwise = [cedecl|typedef $ty:typ $id:tyname;|]

toIndex :: Int -> String
toIndex dim = "toIndexDIM" ++ show dim

fromIndex :: Int -> String
fromIndex dim = "fromIndexDIM" ++ show dim

size :: Int -> String
size dim = "sizeDIM" ++ show dim

mkStruct :: String -> Bool -> [Type] -> Definition
mkStruct name volatile types =
  [cedecl|
   typedef struct {
     $sdecls:fields
   } $id:name ;
  |]
     where
       types' = if volatile then map mkVolatile types else types
       mkDecl :: Type -> String -> FieldGroup
       mkDecl t var = [csdecl|$ty:t $id:var;|]

       names  = reverse $ take (length types) ['a' : show v | v <- [0..]]
       fields = zipWith mkDecl types' names

mkDeviceFun :: String -> Type -> [Param] -> Exp -> Definition
mkDeviceFun name tyout args expr =
  [cedecl| inline $ty:tyout $id:name($params:args)
           {
             $ty:tyout r = { $exp:expr };
             return r;
           }
  |]

mkDeviceFun' :: String -> Type -> [Param] -> [Stm] -> Definition
mkDeviceFun' name tyout args body =
  [cedecl| inline $ty:tyout $id:name($params:args)
           {
             $stms:body
           }
  |]
