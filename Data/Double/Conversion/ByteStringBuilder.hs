-- |
-- Module      : Data.Double.Conversion.ByteStringBuilder
-- Copyright   : (c) 2011 MailRank, Inc. , 2018 Rinat Stryungis 
--
-- License     : BSD-style
-- Maintainer  : lazybonesxp@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, efficient support for converting between double precision
-- floating point values and text.
--

module Data.Double.Conversion.ByteStringBuilder 
    (
      toExponential
    , toFixed
    , toPrecision
    , toShortest
    ) where 

import Control.Monad (when)

import Data.ByteString.Builder (Builder (..))
import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boudedPrim)
import Data.ByteString.Builder.Prim (primBounded)

import Data.Double.Conversion.FFI
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CInt)
import Foreign.Ptr (Ptr, plusPtr)

-- | Compute a representation in exponential format with the requested
-- number of digits after the decimal point. The last emitted digit is
-- rounded.  If -1 digits are requested, then the shortest exponential
-- representation is computed.
toExponential :: Int -> Double -> Builder
toExponential ndigits = primBounded $ convertBounded "toExponential" len $ \val mba ->
                        c_ToExponential val mba (fromIntegral ndigits)
  where len = c_ToExponentialLength
        {-# NOINLINE len #-}

-- | Compute @precision@ leading digits of the given value either in
-- exponential or decimal format. The last computed digit is rounded.
toPrecision :: Int -> Double -> Builder
toPrecision ndigits = primBounded $ convertBounded "toPrecision" len $ \val mba ->
                      c_ToPrecision val mba (fromIntegral ndigits)
  where len = c_ToPrecisionLength
        {-# NOINLINE len #-}

-- | Compute the shortest string of digits that correctly represent
-- the input number.
toShortest :: Double -> Builder
toShortest = primBounded $ convertBounded "toShortest" len c_ToShortest
  where len = c_ToShortestLength
        {-# NOINLINE len #-}

-- | Compute a decimal representation with a fixed number of digits
-- after the decimal point. The last emitted digit is rounded.
toFixed :: Int -> Double -> Builder
toFixed ndigits = primBounded $ convertBounded "toFixed" len $ \val mba ->
                  c_ToFixed val mba (fromIntegral ndigits)
  where len = c_ToFixedLength
        {-# NOINLINE len #-}


convertBounded :: String -> CInt -> (CDouble -> Ptr Word8 -> IO CInt) -> BoundedPrim Double
convertBounded func len act = boudedPrim (fromIntegral len) $ \val ptr -> do 
  size <- act (realToFrac val) ptr
  when (size == -1) .
    fail $ "Data.Double.Conversion.ByteString." ++ func ++
           ": conversion failed (invalid precision requested)"
  return (ptr `plusPtr` (fromIntegral size))