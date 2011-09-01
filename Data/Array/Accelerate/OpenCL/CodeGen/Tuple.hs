{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      : Data.Array.Accelerate.OpenCL.CodeGen.Tuple
-- Copyright   : [2011] Martin Dybdal
-- License     : BSD3
--
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.OpenCL.CodeGen.Tuple
  (
    mkInputTuple, mkOutputTuple, Accessor (..),
    mkTupleTypeAsc
    --    mkTupleType, mkTuplePartition
  )
  where

import Data.Maybe

-- Quasiquotation library
import Language.C.Quote.OpenCL
import Language.C hiding (mkPtr)
import qualified Language.C.Syntax
import qualified Data.Loc
import qualified Data.Symbol

import Data.Array.Accelerate.OpenCL.CodeGen.Monad
import Control.Monad

import Data.Array.Accelerate.OpenCL.CodeGen.Util

data Accessor = Get (String -> Exp)
              | Set (String -> String -> Exp)

mkInputTuple :: String -> [Type]-> CGM Accessor
mkInputTuple subscript types = mkTupleType (Just subscript) types

mkOutputTuple :: [Type]-> CGM Accessor
mkOutputTuple types = mkTupleType Nothing types

mkTupleType :: Maybe String -> [Type] -> CGM Accessor
mkTupleType subscript types = do
  let n = length types
      tuple_name = maybe "TyOut" ("TyIn" ++) subscript
      volatile = isNothing subscript
      tynames
        | n > 1     = take n [tuple_name ++ "_" ++ show i | i <- [0..]] -- TyInA_0, TyInA_1, ...
        | otherwise = [tuple_name]

  addDefinitions $ zipWith (mkTypedef volatile) tynames types
  accessorCall <- mkParameterList subscript n tynames
  (maybe mkSet mkGet subscript) n tynames
  when (n > 1) $ addDefinition (mkStruct tuple_name volatile types)
  return accessorCall

mkTupleTypeAsc :: Int -> [Type] -> CGM [Accessor]
mkTupleTypeAsc n types = do
  accessorOut <- mkOutputTuple types
  accessorsIn <- mkInputTuples (n-1)
  return $ accessorOut : accessorsIn
  where
    mkInputTuples 0 = return []
    mkInputTuples n = do
      as <- mkInputTuples (n-1)
      a <- mkInputTuple (show $ n-1) types
      return $ a : as

mkParameterList :: Maybe String -> Int -> [String] -> CGM Accessor
mkParameterList subscript n tynames = do
  addParams $ params (zip types' param_names)
  return accessorCall
  where
    param_prefix = maybe "out" ("in" ++) subscript
    param_names
      | n > 1     = take n [param_prefix ++ "_" ++ show i | i <- [0..]] -- inA_0, inB_0, ..
      | otherwise = [param_prefix] -- inA or out
    types' = map (mkPtr . mkGlobal . typename) tynames

    args = map (\p -> [cexp|$id:p|]) param_names
    accessorCall =
      case subscript of
        Nothing -> Set $ \idx val -> [cexp|set($id:idx, $id:val, $args:args)|]
        Just x  -> Get $ \idx -> [cexp|$id:("get" ++ x)($id:idx, $args:args)|]

mkGet :: String -> Int -> [String] -> CGM ()
mkGet prj n tynames = do
  params <- getParams
  let name = "get" ++ prj
      param_name = "in" ++ prj
      returnType = typename $ "TyIn" ++ prj
      assign i name = let field = 'a' : show i
                      in [cstm|val.$id:field = $id:name [idx];|]
      assignments
        | n > 1     = zipWith assign [0..] tynames
        | otherwise = [ [cstm|val = $id:param_name [idx];|] ]

  addDefinition
     [cedecl|
       inline $ty:returnType $id:name(const $ty:ix idx, $params:params) {
         $ty:returnType val;
         $stms:assignments
         return val;
       }
     |]

mkSet :: Int -> [String] -> CGM ()
mkSet n tynames = do
  params <- getParams
  let assign i name = let field = 'a' : show i
                      in [cstm|$id:name [idx] = val.$id:field;|]
      assignments
        | n > 1     = zipWith assign [0..] tynames
        | otherwise = [ [cstm|out[idx] = val;|] ]
  addDefinition
     [cedecl|
       inline void set(const $ty:ix idx, const $ty:outType val, $params:params) {
         $stms:assignments
       }
     |]

-- TODO partition
-- TODO understand difference between mkTupleType and mkTupleTypeAsc

-- -- A variant of tuple generation for associative array computations, generating
-- -- base get and set functions, and the given number of type synonyms.
-- --
-- mkTupleTypeAsc :: Int -> [CType] -> [CExtDecl]
-- mkTupleTypeAsc syn ty = types ++ synonyms ++ [mkSet n, mkGet n 0]
--   where
--     n        = length ty
--     synonyms = concat . take syn . flip map ([0..] :: [Int]) $ \v ->
--       [ mkTypedef ("TyIn"  ++ show v) False False [CTypeDef (internalIdent "TyOut")  internalNode]
--       , mkTypedef ("ArrIn" ++ show v) False False [CTypeDef (internalIdent "ArrOut") internalNode] ]
--     types
--       | n <= 1    = [ mkTypedef "TyOut" False False (head ty), mkTypedef "ArrOut" True True (head ty)]
--       | otherwise = [ mkStruct  "TyOut" False False ty,        mkStruct  "ArrOut" True True ty]


-- mkTuplePartition :: String -> [CType] -> Bool -> CExtDecl
-- mkTuplePartition tyName ty isVolatile =
--   CFDefExt
--     (CFunDef
--       [CStorageSpec (CStatic internalNode),CTypeQual (CInlineQual internalNode),CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)),CTypeSpec (CTypeDef (internalIdent tyName) internalNode)]
--       (CDeclr (Just (internalIdent "partition")) [CFunDeclr (Right ([CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CVoidType internalNode)] [(Just (CDeclr (Just (internalIdent "s_data")) [CPtrDeclr [] internalNode] Nothing [] internalNode),Nothing,Nothing)] internalNode,CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CIntType internalNode)] [(Just (CDeclr (Just (internalIdent "n")) [] Nothing [] internalNode),Nothing,Nothing)] internalNode],False)) [] internalNode] Nothing [] internalNode)
--       []
--       (CCompound [] (stmts ++ [CBlockDecl (CDecl [CTypeSpec (CTypeDef (internalIdent tyName) internalNode)] [(Just (CDeclr (Just (internalIdent "r")) [] Nothing [] internalNode),Just initp,Nothing)] internalNode) ,CBlockStmt (CReturn (Just (CVar (internalIdent "r") internalNode)) internalNode)]) internalNode)
--       internalNode)
--   where
--     n     = length ty
--     var s = CVar (internalIdent s) internalNode
--     names = map (('p':) . show) [n-1,n-2..0]
--     initp = mkInitList (map var names)
--     volat = [CTypeQual (CVolatQual internalNode) | isVolatile]
--     stmts = zipWith  (\l r -> CBlockDecl (CDecl (volat ++ map CTypeSpec l) r internalNode)) ty
--           . zipWith3 (\p t s -> [(Just (CDeclr (Just (internalIdent p)) [CPtrDeclr [] internalNode] Nothing [] internalNode),Just (CInitExpr (CCast (CDecl (map CTypeSpec t) [(Just (CDeclr Nothing [CPtrDeclr [] internalNode] Nothing [] internalNode),Nothing,Nothing)] internalNode) s internalNode) internalNode),Nothing)]) names ty
--           $ var "s_data" : map (\v -> CUnary CAdrOp (CIndex (var v) (CVar (internalIdent "n") internalNode) internalNode) internalNode) names

