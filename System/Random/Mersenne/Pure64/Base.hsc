{-# LANGUAGE EmptyDataDecls, CPP, ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module     : System.Random.Mersenne.Pure64
-- Copyright  : Copyright (c) 2008, Don Stewart <dons@galois.com>
-- License    : BSD3
-- Maintainer : Don Stewart <dons@galois.com>
-- Stability  : experimental
-- Portability: CPP, FFI, EmptyDataDecls
-- Tested with: GHC 6.8.2
--
-- A purely functional binding 64 bit binding to the classic mersenne
-- twister random number generator. This is more flexible than the 
-- impure 'mersenne-random' library, at the cost of being much slower.
-- This generator is however, many times faster than System.Random,
-- and yields high quality randoms with a long period.
--
module System.Random.Mersenne.Pure64.Base where

#include "mt19937-64.h"
#include "mt19937-64-unsafe.h"

import Foreign.C.Types
import Foreign

data MTState

type UInt64 = CULLong

------------------------------------------------------------------------
-- pure version:

foreign import ccall unsafe "init_genrand64"
    c_init_genrand64  :: Ptr MTState -> UInt64 -> IO ()

foreign import ccall unsafe "genrand64_int64"
    c_genrand64_int64 :: Ptr MTState -> IO UInt64

foreign import ccall unsafe "genrand64_real2"
    c_genrand64_real2 :: Ptr MTState -> IO CDouble

------------------------------------------------------------------------
-- model: (for testing purposes)

foreign import ccall unsafe "init_genrand64_unsafe"
    c_init_genrand64_unsafe :: UInt64 -> IO ()

foreign import ccall unsafe "genrand64_int64_unsafe"
    c_genrand64_int64_unsafe :: IO UInt64

foreign import ccall unsafe "genrand64_real2_unsafe"
    c_genrand64_real2_unsafe :: IO CDouble

foreign import ccall unsafe "string.h memcpy"
    c_memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)
