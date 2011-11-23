-- |
-- Module      : Data.Array.Accelerate.OpenCL.Config
-- Copyright   : [2011] Martin Dybdal
-- License     : BSD3
--
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.OpenCL.Config where


data DeviceProps = DeviceProps
    { warpSize :: Int
    }


defaultConfig :: DeviceProps
defaultConfig = DeviceProps { warpSize = 32 }
