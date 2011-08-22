module Data.Array.Accelerate.OpenCL (
  Arrays, run, stream
) where

import System.IO.Unsafe

import Data.Array.Accelerate.AST                  (Arrays(..), ArraysR(..))
import Data.Array.Accelerate.Smart                (Acc, convertAcc, convertAccFun1)
import Data.Array.Accelerate.Array.Representation (size)
import Data.Array.Accelerate.Array.Sugar          (Array(..))


-- | Compile and run a complete embedded array program using the OpenCL backend
--
run :: Arrays a => Acc a -> a
{-# NOINLINE run #-}
run a = unsafePerformIO execute
  where
    acc     = convertAcc a
    execute = undefined --evalOpenCL (compileAcc acc >>= executeAcc >>= collect)

stream = undefined