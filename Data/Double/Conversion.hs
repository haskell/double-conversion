{-# LANGUAGE ForeignFunctionInterface, MagicHash, Rank2Types,
    UnliftedFFITypes #-}

module Data.Double.Conversion
    (
      toExponential
    , toFixed
    , toPrecision
    , toShortest
    ) where

import Control.Monad (when)
import Control.Monad.ST (unsafeIOToST, runST)
import Data.Text.Internal (Text(Text))
import Foreign.C.Types (CDouble, CInt)
import GHC.Prim (MutableByteArray#)
import qualified Data.Text.Array as A

toExponential :: Int -> Double -> Text
toExponential ndigits = convert len $ \val mba ->
                        c_ToExponential val mba (fromIntegral ndigits)
  where len = c_ToExponentialLength
        {-# NOINLINE len #-}

toFixed :: Int -> Double -> Text
toFixed ndigits = convert len $ \val mba ->
                  c_ToFixed val mba (fromIntegral ndigits)
  where len = c_ToFixedLength
        {-# NOINLINE len #-}

toShortest :: Double -> Text
toShortest = convert len c_ToShortest
  where len = c_ToShortestLength
        {-# NOINLINE len #-}

toPrecision :: Int -> Double -> Text
toPrecision ndigits = convert len $ \val mba ->
                      c_ToPrecision val mba (fromIntegral ndigits)
  where len = c_ToPrecisionLength
        {-# NOINLINE len #-}

convert :: CInt -> (forall s. CDouble -> MutableByteArray# s -> IO CInt)
        -> Double -> Text
convert len act val = runST go
  where
    go = do
      buf <- A.new (fromIntegral len)
      size <- unsafeIOToST $ act (realToFrac val) (A.maBA buf)
      when (size == -1) $ fail "convert"
      frozen <- A.unsafeFreeze buf
      return $! Text frozen 0 (fromIntegral size)

foreign import ccall unsafe "hs-double-conversion.h _hs_ToShortestLength"
    c_ToShortestLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToShortest"
    c_ToShortest :: CDouble -> MutableByteArray# s -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToFixedLength"
    c_ToFixedLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToFixed"
    c_ToFixed :: CDouble -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToExponentialLength"
    c_ToExponentialLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToExponential"
    c_ToExponential :: CDouble -> MutableByteArray# s -> CInt -> IO CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToPrecisionLength"
    c_ToPrecisionLength :: CInt

foreign import ccall unsafe "hs-double-conversion.h _hs_ToPrecision"
    c_ToPrecision :: CDouble -> MutableByteArray# s -> CInt -> IO CInt
