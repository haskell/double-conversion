{-# LANGUAGE ForeignFunctionInterface, MagicHash, UnliftedFFITypes #-}

-- |
-- Module      : Data.Double.Conversion.FFI
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@mailrank.com
-- Stability   : experimental
-- Portability : GHC
--
-- FFI interface support for converting between double precision
-- floating point values and text.

module Data.Double.Conversion.FFI
    (
      c_Text_ToExponential
    , c_Text_ToFixed
    , c_Text_ToPrecision
    , c_Text_ToShortest
    , c_ToExponentialLength
    , c_ToFixedLength
    , c_ToPrecisionLength
    , c_ToShortestLength
    ) where

import Foreign.C.Types (CDouble, CInt)
import GHC.Prim (MutableByteArray#)

foreign import ccall unsafe "hs-double-conversion.h _hs_ToShortestLength"
    c_ToShortestLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToShortest"
    c_Text_ToShortest :: CDouble -> MutableByteArray# s -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToFixedLength"
    c_ToFixedLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToFixed"
    c_Text_ToFixed :: CDouble -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToExponentialLength"
    c_ToExponentialLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToExponential"
    c_Text_ToExponential :: CDouble -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToPrecisionLength"
    c_ToPrecisionLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToPrecision"
    c_Text_ToPrecision :: CDouble -> MutableByteArray# s -> CInt -> IO CInt
