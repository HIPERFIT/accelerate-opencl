{-# LANGUAGE CPP, GADTs, RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.OpenCL.Analysis.Launch
-- Copyright   : [2011] Martin Dybdal
-- License     : BSD3
--
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.OpenCL.Analysis.Launch (launchConfig)
  where

-- friends
import Data.Array.Accelerate.AST                        (PreOpenAcc)
import Data.Array.Accelerate.OpenCL.State               (CIO)
import Data.Array.Accelerate.OpenCL.Compile             (ExecOpenAcc(..))

-- library
import Control.Monad.IO.Class (liftIO)

import qualified Foreign.OpenCL.Bindings                as OpenCL
import qualified Foreign.Storable                       as F

-- | Find a suitable launch configuration, we first pick a valid work
-- group size and then round up the global size such that the work
-- group size is a divisor.
--
-- The CUDA backend chooses another approach, and we therefore have to
-- account for this in the OpenCL kernels
launchConfig :: OpenCL.DeviceID -> PreOpenAcc ExecOpenAcc aenv a -> Int -> OpenCL.Kernel -> CIO (Int, Int, Integer)
launchConfig dev acc n fn = liftIO $ do
  maxWorkGroupSize <- OpenCL.deviceMaxWorkGroupSize dev
  maxWorkItemSizes <- OpenCL.deviceMaxWorkItemSizes dev
  -- Pick a groupSize that satisfies device constraints
  let groupSize = minimum [n,
                           fromIntegral maxWorkGroupSize,
                           fromIntegral $ maxWorkItemSizes !! 0]
      globalSize = shrRoundUp groupSize n
  return (groupSize, globalSize, 0)

-- Round up the second argument such that the first argument is a
-- divisor
shrRoundUp :: Int -> Int -> Int
shrRoundUp 0 _ = 0
shrRoundUp groupSize globalSize =
  let r = globalSize `mod` groupSize
  in if r == 0
     then globalSize
     else globalSize + groupSize - r;
