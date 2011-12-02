{-# LANGUAGE CPP, GADTs, PatternGuards, TemplateHaskell #-}
{-# LANGUAGE TupleSections, TypeFamilies, TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.OpenCL.State
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- This module defines a state monad token which keeps track of the code
-- generator state, including memory transfers and external compilation
-- processes.
--

module Data.Array.Accelerate.OpenCL.State (

  evalOpenCL, runOpenCL, runOpenCLWith, CIO,
  OpenCLState, unique, cl_platform , cl_devices , cl_context,
  memoryTable, kernelTable,

  KernelTable, KernelEntry(KernelEntry), kernelName, kernelStatus,
  MemoryEntry(..), AccArrayData(..), refcount, newAccMemoryTable

) where

-- friends
import Data.Array.Accelerate.OpenCL.Analysis.Device
import Data.Array.Accelerate.OpenCL.Analysis.Hash
import qualified Data.Array.Accelerate.Array.Data       as AD

-- library
import Data.Int
import Data.Word
import Data.IORef
import Data.Maybe
import Data.List
import Data.Typeable
import Data.Record.Label
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict                       (StateT(..))
import System.Posix.Types                               (ProcessID)
import Control.Concurrent.MVar
--import System.Mem.Weak
import System.IO.Unsafe
import Foreign.Ptr

import qualified Data.HashTable                         as Hash

import Foreign.OpenCL.Bindings

#ifdef ACCELERATE_OPENCL_PERSISTENT_CACHE
import System.Directory
import System.FilePath
import Data.Binary                                      (encodeFile, decodeFile)
import Control.Arrow                                    (second)
import Paths_accelerate_opencl                          (getDataDir)
#endif

#include "accelerate.h"


-- An exact association between an accelerate computation and its
-- implementation, which is either a reference to the external compiler (nvcc)
-- or the resulting binary module. This is keyed by a string representation of
-- the generated kernel code.
--
-- An Eq instance of Accelerate expressions does not facilitate persistent
-- caching.
--
type KernelTable = Hash.HashTable AccKey KernelEntry
data KernelEntry = KernelEntry
  {
    _kernelName   :: FilePath,
    _kernelStatus :: Either (MVar Program) Program
  }

-- Associations between host- and device-side arrays, with reference counting.
-- Facilitates reuse and delayed allocation at the cost of explicit release.
--
-- This maps to a single concrete array. Arrays of tuples, which are represented
-- internally as tuples of arrays, will generate multiple entries.
--
type MemoryTable = Hash.HashTable AccArrayData MemoryEntry

data AccArrayData where
  AccArrayData :: (Typeable a, AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e)
               => AD.ArrayData e
               -> AccArrayData

instance Eq AccArrayData where
  AccArrayData ad1 == AccArrayData ad2
    | Just p1 <- gcast (AD.ptrsOfArrayData ad1) = p1 == AD.ptrsOfArrayData ad2
    | otherwise                                 = False

data MemoryEntry where
  MemoryEntry :: Typeable a
              => Maybe Int         -- if Nothing, the array is not released by 'freeArray'
              -> MemObject a
              -> MemoryEntry

newAccMemoryTable :: IO MemoryTable
newAccMemoryTable = Hash.new (==) hashAccArray
  where
    hashAccArray :: AccArrayData -> Int32
    hashAccArray (AccArrayData ad) = fromIntegral . ptrToIntPtr
                                   $ AD.ptrsOfArrayData ad

refcount :: MemoryEntry :-> Maybe Int
refcount = lens get set
  where
    get   (MemoryEntry c _) = c
    set c (MemoryEntry _ p) = MemoryEntry c p


-- The state token for accelerated OpenCL array operations
--
-- TLM: the memory table is not persistent between computations. Move elsewhere?
--
--
type CIO = StateT OpenCLState IO

data OpenCLState = OpenCLState
    { _unique      :: Int
    , _cl_platform :: PlatformID
    , _cl_devices  :: [(DeviceID, CommandQueue)]
    , _cl_context  :: Context
    , _kernelTable :: KernelTable
    , _memoryTable :: MemoryTable
    }

$(mkLabels [''OpenCLState, ''KernelEntry])


-- Execution State
-- ---------------

#ifdef ACCELERATE_OPENCL_PERSISTENT_CACHE
indexFileName :: IO FilePath
indexFileName = do
  tmp <- (</> "cache") `fmap` getDataDir
  dir <- createDirectoryIfMissing True tmp >> canonicalizePath tmp
  return (dir </> "_index")
#endif

-- Store the kernel module map to file
--
saveIndexFile :: OpenCLState -> IO ()
#ifdef ACCELERATE_OPENCL_PERSISTENT_CACHE
saveIndexFile s = do
  ind <- indexFileName
  encodeFile ind . map (second _kernelName) =<< Hash.toList (_kernelTable s)
#else
saveIndexFile _ = return ()
#endif

-- Read the kernel index map file (if it exists), loading modules into the
-- current context
--
loadIndexFile :: IO (KernelTable, Int)
#ifdef ACCELERATE_OPENCL_PERSISTENT_CACHE
loadIndexFile = do
  f <- indexFileName
  x <- doesFileExist f
  e <- if x then mapM reload =<< decodeFile f
            else return []
  (,length e) <$> Hash.fromList hashAccKey e
  where
    reload (k,n) = (k,) . KernelEntry n . Right <$> CUDA.loadFile (n `replaceExtension` ".clbin")
#else
loadIndexFile = (,0) <$> Hash.new (==) hashAccKey
#endif

-- Selects the OpenCL platform with the highest compute capacity and
-- build OpenCL-state from the devices of this platform
initialise :: IO OpenCLState
initialise = do
  (platform, devices) <- selectBestPlatform
  context <- createContext devices [ContextPlatform platform] NoContextCallback
  queues <- mapM (\dev -> queueProps dev >>= createCommandQueue context dev) devices
  (knl, u) <- loadIndexFile
  return $ OpenCLState
    { _unique = u
    , _cl_platform = platform
    , _cl_devices = zip devices queues
    , _cl_context = context
    , _kernelTable = knl
    , _memoryTable = error "Memory table not initialized"
    }

queueProps :: DeviceID -> IO [CommandQueueProperties]
queueProps dev = do
  supported <- deviceQueueProperties dev
  return $ intersect supported [QueueOutOfOrderExecModeEnable]

-- | Evaluate a OpenCL array computation under the standard global environment
--
evalOpenCL :: CIO a -> IO a
evalOpenCL = liftM fst . runOpenCL

runOpenCL :: CIO a -> IO (a, OpenCLState)
runOpenCL acc = readIORef onta >>= flip runOpenCLWith acc


-- | Execute a computation under the provided state, returning the updated
-- environment structure and replacing the global state.
--
runOpenCLWith :: OpenCLState -> CIO a -> IO (a, OpenCLState)
runOpenCLWith state acc = do
  (a,s) <- runStateT acc state
  saveIndexFile s
  writeIORef onta =<< sanitise s
  return (a,s)
  where
    -- The memory table and compute table are transient data structures: they
    -- exist only for the life of a single computation [stream]. Don't record
    -- them into the persistent state token.
    --
    sanitise :: OpenCLState -> IO OpenCLState
    sanitise st = do
      entries <- filter (isJust . getL refcount . snd) <$> Hash.toList (getL memoryTable st)
      INTERNAL_ASSERT "runOpenCL.sanitise" (null entries)
        $ return (setL memoryTable undefined st)


-- Nasty global statesses
-- ----------------------

-- hic sunt dracones: truly unsafe use of unsafePerformIO
--
onta :: IORef OpenCLState
{-# NOINLINE onta #-}
onta = unsafePerformIO (initialise >>= newIORef)
