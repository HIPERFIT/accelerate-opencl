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
    mkInputTuple, mkOutputTuple, --Accessor (..),
    mkTupleTypeAsc, Arguments,
    mkParameterList
    --    mkTupleType, mkTuplePartition
  )
  where

import Data.Maybe
import Data.Char

-- Quasiquotation library
import Language.C.Quote.OpenCL
import Language.C hiding (mkPtr)
import qualified Language.C.Syntax
import qualified Data.Loc
import qualified Data.Symbol

import Data.Array.Accelerate.OpenCL.CodeGen.Monad
import Control.Monad

import Data.Array.Accelerate.OpenCL.CodeGen.Util

-- data Accessor = Get (String -> Exp)
--               | Set (String -> String -> Exp)


type Arguments = [Exp]

mkInputTuple :: String -> [Type]-> CGM Arguments
mkInputTuple subscript = mkTupleType (Just subscript)

mkOutputTuple :: [Type]-> CGM Arguments
mkOutputTuple = mkTupleType Nothing

 -- ..., TyInA_1, TyInA_0
createTypenames name n | n > 1     = reverse $ take n [name ++ "_" ++ show i | i <- [0..]]
                       | otherwise = [name]

mkTupleType :: Maybe String -> [Type] -> CGM Arguments
mkTupleType subscript types = do
  let n = length types
      tuple_name = maybe "TyOut" ("TyIn" ++) subscript
      volatile = False --isNothing subscript
      tynames = createTypenames tuple_name n
  addDefinitions $ zipWith (mkTypedef volatile) tynames types
  when (n > 1) $ addDefinition (mkStruct tuple_name volatile $ map typename tynames)
  (args,ps) <- mkParameterList Global subscript n tynames
  (_,psLocal) <- mkParameterList Local subscript n tynames
  (maybe mkSet mkGet subscript) n ps Global
  (maybe mkSet mkGet subscript) n psLocal Local
  addParams ps
  return args

mkInputTypedef :: String -> Int -> CGM Arguments
mkInputTypedef subscript n = do
  let tuple_name = "TyIn" ++ subscript
      tynames_in = createTypenames tuple_name n
      tynames_out = createTypenames "TyOut" n
  addDefinitions $ zipWith (mkTypedef True) tynames_in $ map typename tynames_out
  when (n > 1) $ addDefinition $ mkTypedef False tuple_name (typename "TyOut")
  (args,ps) <- mkParameterList Global (Just subscript) n tynames_in
  (_,psLocal) <- mkParameterList Local (Just subscript) n tynames_in
  mkGet subscript n ps Global
  mkGet subscript n psLocal Local
  addParams ps
  return args

mkTupleTypeAsc :: Int -> [Type] -> CGM (Arguments, [Arguments])
mkTupleTypeAsc cargs typ = do
  argsOut <- mkOutputTuple typ
  let names = [ [chr $ ord 'A' + i] | i <- [0..cargs-1]]
  argsIn <- mapM (flip mkInputTypedef $ length typ) names
  return $ (argsOut, argsIn)

-- mkLocalAccessors :: Int -> [Type] -> CGM ()
-- mkLocalAccessors subscript types = do
--   let names = [ [chr $ ord 'A' + i] | i <- [0..n-1]]
--       n = length types
--       tynames
--         | n > 1     = take n [tuple_name ++ "_" ++ show i | i <- [0..]] -- TyInA_0, TyInA_1, ...
--         | otherwise = [tuple_name]
--   (argsOut, psout) <-  mkParameterList Local Nothing n
--   argsIn <- mapM (flip mkInputTuple typ) names
--   return $ (argsOut, argsIn)

mkParameterList :: StorageQual -> Maybe String -> Int -> [String] -> CGM (Arguments, [Param])
mkParameterList storage subscript n tynames = do
  let ps = params (zip types' param_names)
  return (args, ps)
   where
    param_prefix = maybe "out" ("in" ++) subscript
    param_names = createTypenames param_prefix n
    types' = map (mkPtr . changeStorage storage . typename) tynames

    args = map (\p -> [cexp|$id:p|]) param_names

mkGet :: String -> Int -> [Param] -> StorageQual -> CGM ()
mkGet prj n params storage = do
  addDefinition
     [cedecl|
       inline $ty:returnType $id:name($ty:ix idx, $params:params) {
         $ty:returnType val;
         $stms:assignments
         return val;
       }
     |]
   where
     parnames = ["in" ++ prj ++ "_" ++ show i | i <- [0..]]
     name | storage == Local = "get" ++ prj ++ "_local"
          | otherwise = "get" ++ prj
     returnType = typename $ "TyIn" ++ prj
     assign i name = let field = 'a' : show i
                     in [cstm|val.$id:field = $id:name [idx];|]
     assignments
      | n > 1     = take n $ zipWith assign [0..] parnames
      | otherwise = [ [cstm|val = $id:("in" ++ prj) [idx];|] ]


mkSet :: Int -> [Param] -> StorageQual -> CGM ()
mkSet n params storage =
  addDefinition
     [cedecl|
       inline void $id:name($ty:ix idx, const $ty:outType val, $params:params) {
         $stms:assignments
       }
     |]
   where
     name | storage == Local = "set_local"
          | otherwise = "set"
     parnames = ["out" ++ "_" ++ show i | i <- [0..]]
     assign i name = let field = 'a' : show i
                     in [cstm|$id:name [idx] = val.$id:field;|]
     assignments
      | n > 1     = take n $ zipWith assign [0..] parnames
      | otherwise = [ [cstm|out[idx] = val;|] ]

-- TODO partition

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

