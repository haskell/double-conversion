{-# LANGUAGE CPP, ForeignFunctionInterface, MagicHash, TypeFamilies,
             UnliftedFFITypes #-}

-- |
-- Module      : Data.Double.Conversion.FFI
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- FFI interface support for converting between
-- floating point values and text.

module Data.Double.Conversion.FFI
    (
      ForeignFloating (..)
    , c_Text_ToExponential
    , c_Text_ToExponentialFloat
    , c_Text_ToFixed
    , c_Text_ToFixedFloat
    , c_Text_ToPrecision
    , c_Text_ToPrecisionFloat
    , c_Text_ToShortest
    , c_Text_ToShortestFloat
    , c_ToExponentialLength
    , c_ToFixedLength
    , c_ToPrecisionLength
    , c_ToShortestLength
    , c_ToExponential
    , c_ToExponentialFloat
    , c_ToFixed
    , c_ToFixedFloat
    , c_ToPrecision
    , c_ToPrecisionFloat
    , c_ToShortest
    , c_ToShortestFloat
    ) where

import Data.Word (Word8)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CDouble(CDouble), CFloat(CFloat), CInt(CInt))
#else
import Foreign.C.Types (CDouble, CFloat, CInt)
#endif
import Foreign.Ptr (Ptr)
import GHC.Prim (MutableByteArray#)

type family ForeignFloating h :: *

type instance ForeignFloating Double = CDouble
type instance ForeignFloating Float = CFloat

foreign import ccall unsafe "hs-double-conversion.h _hs_ToShortestLength"
    c_ToShortestLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToShortest"
    c_Text_ToShortest :: CDouble -> MutableByteArray# s -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToShortest"
    c_ToShortest :: CDouble -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToShortestFloat"
    c_Text_ToShortestFloat :: CFloat -> MutableByteArray# s -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToShortestFloat"
    c_ToShortestFloat :: CFloat -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToFixedLength"
    c_ToFixedLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToFixed"
    c_Text_ToFixed :: CDouble -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToFixedFloat"
    c_Text_ToFixedFloat :: CFloat -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToFixed"
    c_ToFixed :: CDouble -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToFixedFloat"
    c_ToFixedFloat :: CFloat -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToExponentialLength"
    c_ToExponentialLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToExponential"
    c_Text_ToExponential :: CDouble -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToExponential"
    c_ToExponential :: CDouble -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToExponentialFloat"
    c_Text_ToExponentialFloat :: CFloat -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToExponentialFloat"
    c_ToExponentialFloat :: CFloat -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToPrecisionLength"
    c_ToPrecisionLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToPrecision"
    c_Text_ToPrecision :: CDouble -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToPrecision"
    c_ToPrecision :: CDouble -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_Text_ToPrecisionFloat"
    c_Text_ToPrecisionFloat :: CFloat -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToPrecisionFloat"
    c_ToPrecisionFloat :: CFloat -> Ptr Word8 -> CInt -> IO CInt
