-- |
-- Module      : Data.Array.Accelerate.OpenCL
-- Copyright   : [2011] Martin Dybdal
-- License     : BSD3
--
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements the OpenCL backend for the embedded array language.
--

module Data.Array.Accelerate.OpenCL (
  Arrays, run, stream, test
) where

import System.IO.Unsafe

import Data.Array.Accelerate.AST                  (Arrays(..), ArraysR(..))
import Data.Array.Accelerate.Smart                (Acc, convertAcc, convertAccFun1)
import Data.Array.Accelerate.Array.Representation (size)
import Data.Array.Accelerate.Array.Sugar          (Array(..))

import Data.Array.Accelerate.OpenCL.CodeGen

-- | Compile and run a complete embedded array program using the OpenCL backend
--
run :: Arrays a => Acc a -> a
{-# NOINLINE run #-}
run a = unsafePerformIO execute
  where
    acc     = convertAcc a
    execute = undefined --evalOpenCL (compileAcc acc >>= executeAcc >>= collect)

stream = undefined


test a = codeGenAcc (convertAcc a) []