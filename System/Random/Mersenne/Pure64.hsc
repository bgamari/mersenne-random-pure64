{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module     : System.Random.Mersenne.Pure64
-- Copyright  : Copyright (c) 2008, Don Stewart <dons@galois.com>
-- License    : BSD3
-- Maintainer : Don Stewart <dons@galois.com>
-- Stability  : experimental
-- Portability: CPP, FFI
-- Tested with: GHC 6.8.2
--
-- A purely functional binding 64 bit binding to the classic mersenne
-- twister random number generator. This is more flexible than the 
-- impure 'mersenne-random' library, at the cost of being much slower.
-- This generator is however, many times faster than System.Random,
-- and yields high quality randoms with a long period.
--
module System.Random.Mersenne.Pure64 (

    -- * The random number generator
    PureMT          -- abstract: RandomGen

    -- * Introduction
    , pureMT        -- :: Word -> PureMT
    , newPureMT     -- :: IO PureMT

    -- $instance

    -- * Low level access to the generator

    -- $notes
    , randomInt     -- :: PureMT -> (Int   ,PureMT)
    , randomWord    -- :: PureMT -> (Word  ,PureMT)
    , randomInt64   -- :: PureMT -> (Int64 ,PureMT)
    , randomWord64  -- :: PureMT -> (Word64,PureMT)
    , randomDouble  -- :: PureMT -> (Double,PureMT)

    ) where

------------------------------------------------------------------------

#include "mt19937-64.h"

------------------------------------------------------------------------

import System.Random.Mersenne.Pure64.Base

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 605
import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)
#else
import Foreign.ForeignPtr       (mallocForeignPtrBytes)
#endif

import Foreign.C.Types
import Foreign

import System.CPUTime	( getCPUTime )
import System.Time
import System.IO.Unsafe
import Control.Monad

import System.Random

import Data.Word
import Data.Int

------------------------------------------------------------------------

-- | Create a PureMT generator from a 'Word' seed.
pureMT :: Word -> PureMT
pureMT = init_genrand64 . fromIntegral

-- | Create a new PureMT generator, using the clocktime as the base for the seed.
newPureMT :: IO PureMT
newPureMT = do
    ct             <- getCPUTime
    (TOD sec psec) <- getClockTime
    return $ pureMT (fromIntegral $ sec * 1013904242 + psec + ct)

------------------------------------------------------------------------
-- System.Random interface.

-- $instance 
-- 
-- Being purely functional, the PureMT generator is an instance of
-- RandomGen. However, it doesn't support 'split' yet.

instance RandomGen PureMT where
   next  = randomInt
   split = error "System.Random.Mersenne.Pure: unable to split the mersenne twister"

------------------------------------------------------------------------
-- Direct access to Int, Word and Double types

-- | Yield a new 'Int' value from the generator, returning a new
-- generator and that 'Int'. The full 64 bits will be used on a 64 bit machine.
randomInt :: PureMT -> (Int,PureMT)
randomInt g = (fromIntegral i, g')
        where (i,g') = genrand64_int64 g

-- | Yield a new 'Word' value from the generator, returning a new
-- generator and that 'Word'. 
randomWord :: PureMT -> (Word,PureMT)
randomWord g = (fromIntegral i, g')
        where (i,g') = genrand64_int64 g

-- | Yield a new 'Int64' value from the generator, returning a new
-- generator and that 'Int64'. 
randomInt64 :: PureMT -> (Int64,PureMT)
randomInt64 g = (fromIntegral i, g')
        where (i,g') = genrand64_int64 g

-- | Yield a new 'Word64' value from the generator, returning a new
-- generator and that 'Word64'. 
randomWord64 :: PureMT -> (Word64,PureMT)
randomWord64 g = (fromIntegral i, g')
        where (i,g') = genrand64_int64 g

-- | Efficiently yield a new 53-bit precise 'Double' value, and a new generator.
randomDouble :: PureMT -> (Double,PureMT)
randomDouble g = (realToFrac i, g')
        where (i,g') = genrand64_real2 g

------------------------------------------------------------------------
-- We can have only one mersenne supply in a program.

-- You have to commit at initialisation time to call either
-- rand_gen32 or rand_gen64, and correspondingly, real2 or res53
-- for doubles.
--

-- | 'PureMT', a pure mersenne twister pseudo-random number generator
--
newtype PureMT  = PureMT (ForeignPtr MTState)

instance Show PureMT where
    show _ = show "<PureMT>"

sizeof_MTState :: Int
sizeof_MTState = (#const (sizeof (struct mt_state_t))) -- 2504 bytes

------------------------------------------------------------------------
-- Low level interface

init_genrand64 :: UInt64 -> PureMT
init_genrand64 seed = unsafePerformIO $ do
    fp <- mallocFastBytes sizeof_MTState
    withForeignPtr fp $ \p -> c_init_genrand64 p seed -- fill it
    return $ PureMT fp

genrand64_int64 :: PureMT -> (UInt64, PureMT)
genrand64_int64 o   = unsafePerformIO $ do
    PureMT n <- copyPureMT o
    v        <- withForeignPtr n $ c_genrand64_int64
    n `seq` v `seq`
        return (v,PureMT n)
{-# INLINE genrand64_int64 #-}

genrand64_real2 :: PureMT -> (CDouble, PureMT)
genrand64_real2 o   = unsafePerformIO $ do
    PureMT n <- copyPureMT o
    v        <- withForeignPtr n $ c_genrand64_real2
    n `seq` v `seq`
        return (v,PureMT n)
{-# INLINE genrand64_real2 #-}

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


