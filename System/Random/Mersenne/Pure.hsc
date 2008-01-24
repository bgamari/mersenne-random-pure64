{-# LANGUAGE EmptyDataDecls, CPP, ForeignFunctionInterface, BangPatterns #-}
--------------------------------------------------------------------
-- |
-- Module     : System.Random.Mersenne.Pure
-- Copyright  : Copyright (c) 2008, Don Stewart <dons@galois.com>
-- License    : BSD3
-- Maintainer : Don Stewart <dons@galois.com>
-- Stability  : experimental
-- Portability: CPP, FFI
-- Tested with: GHC 6.8.2
--
-- Generate pseudo-random numbers using the Mersenne Twister
-- pseudorandom number generator. This is a /much/ faster generator than
-- the default 'System.Random' generator for Haskell (~50x faster
-- generation for Doubles, on a core 2 duo), however, it is not 
-- nearly as flexible.
--
module System.Random.Mersenne.Pure where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

#include "mt19937-64.h"
#include "mt19937-64-unsafe.h"

#if __GLASGOW_HASKELL__ >= 605 && !defined(SLOW_FOREIGN_PTR)
import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)
#else
import Foreign.ForeignPtr       (mallocForeignPtrBytes)
#endif

import Foreign.C.Types
import Foreign.C.String
import Foreign

import System.CPUTime	( getCPUTime )
import System.Time
import System.IO.Unsafe
import Control.Monad

import Data.Word
import Data.Int
import Data.Bits
import Data.Char

------------------------------------------------------------------------
-- We can have only one mersenne supply in a program.

-- You have to commit at initialisation time to call either
-- rand_gen32 or rand_gen64, and correspondingly, real2 or res53
-- for doubles.
--

type UInt32 = CUInt
type UInt64 = CULLong

newtype PureMT  = PureMT (ForeignPtr MTState)
    deriving Show -- to do, not referentially transparent

data MTState

sizeof_MTState :: Int
sizeof_MTState = (#const (sizeof (struct mt_state_t))) -- 2504 bytes

------------------------------------------------------------------------

init_genrand64 :: UInt64 -> PureMT
init_genrand64 seed = unsafePerformIO $ do
    fp <- mallocFastBytes sizeof_MTState
    withForeignPtr fp $ \p -> c_init_genrand64 p seed -- fill it
    return (PureMT fp)

genrand64_int64 :: PureMT -> (UInt64, PureMT)
genrand64_int64 o   = unsafePerformIO $ do
    PureMT !n <- copyPureMT o
    !v        <- withForeignPtr n $ c_genrand64_int64
    return (v,PureMT n)

genrand64_real2 :: PureMT -> (CDouble, PureMT)
genrand64_real2 o   = unsafePerformIO $ do
    PureMT !n <- copyPureMT o
    !v        <- withForeignPtr n $ c_genrand64_real2
    return (v,PureMT n)

-- ---------------------------------------------------------------------
-- Memory management

--
-- | Wrapper of mallocForeignPtrBytes with faster implementation
-- Allocate pinned heap memory for the state.
--
mallocFastBytes :: Int -> IO (ForeignPtr a)
mallocFastBytes s = do
#if __GLASGOW_HASKELL__ >= 605 && !defined(SLOW_FOREIGN_PTR)
    mallocPlainForeignPtrBytes s
#else
    mallocForeignPtrBytes s
#endif
{-# INLINE mallocFastBytes #-}

--
-- Duplicate a random state, prior to permuting it.
--
copyPureMT :: PureMT -> IO PureMT
copyPureMT (PureMT oldfp) = do
    newfp <- mallocFastBytes sizeof_MTState
    withForeignPtr newfp $ \p ->
        withForeignPtr oldfp $ \q ->
            c_memcpy (castPtr p) (castPtr q) (fromIntegral sizeof_MTState)
    return $! PureMT newfp
{-# INLINE copyPureMT #-}

------------------------------------------------------------------------

-- | This function initializes the internal state array with a 32-bit integer seed.
foreign import ccall unsafe "init_genrand64"
    c_init_genrand64  :: Ptr MTState -> UInt64 -> IO ()

foreign import ccall unsafe "genrand64_int64"
    c_genrand64_int64 :: Ptr MTState -> IO UInt64

foreign import ccall unsafe "genrand64_real2"
    c_genrand64_real2 :: Ptr MTState -> IO CDouble

------------------------------------------------------------------------
-- model:

foreign import ccall unsafe "init_genrand64_unsafe"
    c_init_genrand64_unsafe :: UInt64 -> IO ()

foreign import ccall unsafe "genrand64_int64_unsafe"
    c_genrand64_int64_unsafe :: IO UInt64

foreign import ccall unsafe "genrand64_real2_unsafe"
    c_genrand64_real2_unsafe :: IO CDouble

------------------------------------------------------------------------

foreign import ccall unsafe "string.h memcpy"
    c_memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)
