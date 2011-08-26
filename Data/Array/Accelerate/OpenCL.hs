{-# LANGUAGE CPP, GADTs #-}
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


-- standard library
import Prelude hiding (catch)
import Data.Record.Label
import Control.Exception
import Control.Applicative
import System.IO.Unsafe
import qualified Data.HashTable                   as Hash


-- friends
import Data.Array.Accelerate.AST                  (Arrays(..), ArraysR(..))
import Data.Array.Accelerate.Smart                (Acc, convertAcc, convertAccFun1)
import Data.Array.Accelerate.Array.Representation (size)
import Data.Array.Accelerate.Array.Sugar          (Array(..))
import Data.Array.Accelerate.OpenCL.Array.Data
import Data.Array.Accelerate.OpenCL.State
import Data.Array.Accelerate.OpenCL.Compile
import Data.Array.Accelerate.OpenCL.Execute

import Data.Array.Accelerate.OpenCL.CodeGen

-- | Compile and run a complete embedded array program using the OpenCL backend
--
run :: Arrays a => Acc a -> a
{-# NOINLINE run #-}
run a = unsafePerformIO execute
  where
    acc     = convertAcc a
    execute = evalOpenCL (compileAcc acc >>= executeAcc >>= collect)

stream = undefined


test a = codeGenAcc (convertAcc a) []


-- Copy from device to host, and decrement the usage counter. This last step
-- should result in all transient arrays having been removed from the device.
--
collect :: Arrays arrs => arrs -> CIO arrs
collect arrs = collectR arrays arrs
  where
    collectR :: ArraysR arrs -> arrs -> CIO arrs
    collectR ArraysRunit         ()                = return ()
    collectR ArraysRarray        arr@(Array sh ad) = peekArray ad (size sh) >> freeArray ad >> return arr
    collectR (ArraysRpair r1 r2) (arrs1, arrs2)    = (,) <$> collectR r1 arrs1 <*> collectR r2 arrs2
