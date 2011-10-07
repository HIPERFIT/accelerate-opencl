-- |
-- Module      : Data.Array.Accelerate.OpenCL.Analysis.Device
-- Copyright   : [2011] Martin Dybdal
-- License     : BSD3
--
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.OpenCL.Analysis.Device
       (selectBestPlatform)
  where

import Data.Word
import Data.List
import Control.Monad

import Foreign.OpenCL.Bindings


-- Select the best of the available OpenCL platforms, and all
-- associated devices. This prefers the platform with the highest
-- maximum throughput. This does not take into account any other
-- factors, such as whether the devices is currently in use by another
-- process.
--
selectBestPlatform :: IO (PlatformID, [DeviceID])
selectBestPlatform = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  maxFlops <- mapM (liftM sum . mapM deviceFlops) devices :: IO [Word32]
  let (platform, devs, _) = head . sortWith third $ zip3 platforms devices maxFlops
  return (platform, devs)

-- Compute the number of flops provided by a device
deviceFlops :: DeviceID -> IO Word32
deviceFlops dev = do
  cores <- deviceMaxComputeUnits dev
  clockFreq <- deviceMaxClockFrequency dev
  return $ cores * clockFreq

-- Sort using a function that projects values to ordinals
sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))

third (_,_, c) = c
