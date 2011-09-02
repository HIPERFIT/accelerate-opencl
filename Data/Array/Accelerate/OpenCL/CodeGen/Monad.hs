module Data.Array.Accelerate.OpenCL.CodeGen.Monad where

import Control.Monad.State

import Language.C hiding (mkPtr)
import Language.C.Syntax
import Language.C.Quote.OpenCL

import Data.Array.Accelerate.OpenCL.CodeGen.Data

data SkelState = SkelState {
                     _definitions :: [Definition]
                   , _params :: [Param]
                 }

emptySkelState = SkelState [] []

type CGM = State SkelState

runCGM :: CGM () -> CUTranslSkel
runCGM st = CUTranslSkel . reverse . _definitions $ execState st emptySkelState


-- Setters
addDefinition :: Definition -> CGM ()
addDefinition def =
  modify $ \s -> s {_definitions = def : (_definitions s)}

addParam :: Param -> CGM ()
addParam param =
  modify $ \s -> s {_params = (_params s) ++ [param]}

addDefinitions :: [Definition] -> CGM ()
addDefinitions = mapM_ addDefinition

addParams :: [Param] -> CGM ()
addParams = mapM_ addParam

-- Getters
getDefinitions :: CGM [Definition]
getDefinitions = gets _definitions

getParams :: CGM [Param]
getParams = gets _params


