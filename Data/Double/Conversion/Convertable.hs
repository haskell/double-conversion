{-# LANGUAGE DefaultSignatures, InstanceSigs, MagicHash, MultiParamTypeClasses,
             TypeFamilies #-}

-- |
-- Module      : Data.Double.Conversion.Convertable
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC

module Data.Double.Conversion.Convertable
    ( Convertable(..)
    ) where
import Data.ByteString.Builder.Prim (primBounded)
import Data.Text (Text)

import Data.Double.Conversion.FFI
import Data.String (IsString)

import qualified Data.ByteString.Builder as BB (Builder)
import qualified Data.ByteString.Internal as B (ByteString(..))
import qualified Data.Double.Conversion.ByteString as CB (convert)
import qualified Data.Double.Conversion.ByteStringBuilder as CBB (convert)
import qualified Data.Double.Conversion.Text as CT (convert)
import qualified Data.Double.Conversion.TextBuilder as CTB (convert)
import qualified Data.Text.Internal.Builder as T (Builder)

-- | Type class for floating data types, that cen be converted, using double-conversion library
--
-- Default instanced convert input to Double and then make Bytestring Builder from it.
--
-- list of functions :
--
-- toExponential:
-- Compute a representation in exponential format with the requested
-- number of digits after the decimal point. The last emitted digit is
-- rounded.  If -1 digits are requested, then the shortest exponential
-- representation is computed.
--
-- toPrecision:
-- Compute @precision@ leading digits of the given value either in
-- exponential or decimal format. The last computed digit is rounded.
--
-- toFixed:
-- Compute a decimal representation with a fixed number of digits
-- after the decimal point. The last emitted digit is rounded.
--
-- toShortest:
-- Compute the shortest string of digits that correctly represent
-- the input number.
--
-- Conversion to text is twice faster than conversion to bytestring
-- Conversion to text via Builder (both in the in case of bytestring and text) in case of single number
-- is much slower, than to text or bytestring directly. (2-3x)
-- But conversion large amount of numbers to text via Builder is much faster than directly (up to 50x).
-- Conversion to text via text builder is a little slower, then via bytestring builder

class (RealFloat a, IsString b) => Convertable a b where
  toExponential :: Int -> a -> b
  default toExponential :: (b ~ BB.Builder) => Int -> a -> b
  toExponential ndigits num = primBounded (CBB.convert "toExponential" len $ \val mba ->
                        c_ToExponential val mba (fromIntegral ndigits)) (realToFrac num :: Double)
      where len = c_ToExponentialLength
            {-# NOINLINE len #-}

  toPrecision :: Int -> a -> b
  default toPrecision :: (b ~ BB.Builder) => Int -> a -> b
  toPrecision ndigits num = primBounded (CBB.convert "toPrecision" len $ \val mba ->
                      c_ToPrecision val mba (fromIntegral ndigits)) (realToFrac num :: Double)
      where len = c_ToPrecisionLength
            {-# NOINLINE len #-}

  toFixed :: Int -> a -> b
  default toFixed :: (b ~ BB.Builder) => Int -> a -> b
  toFixed ndigits num = primBounded (CBB.convert "toFixed" len $ \val mba ->
                  c_ToFixed val mba (fromIntegral ndigits)) (realToFrac num :: Double)
      where len = c_ToFixedLength
            {-# NOINLINE len #-}

  toShortest :: a -> b
  default toShortest :: (b ~ BB.Builder) => a -> b
  toShortest num = primBounded (CBB.convert "toShortest" len c_ToShortest) (realToFrac num :: Double)
      where len = c_ToShortestLength
            {-# NOINLINE len #-}

-- Instances

instance Convertable Double BB.Builder where
    toExponential :: Int -> Double -> BB.Builder
    toExponential ndigits = primBounded $ CBB.convert "toExponential" len $ \val mba ->
                        c_ToExponential val mba (fromIntegral ndigits)
        where len = c_ToExponentialLength
              {-# NOINLINE len #-}

    toPrecision :: Int -> Double -> BB.Builder
    toPrecision ndigits = primBounded (CBB.convert "toPrecision" len $ \val mba ->
                      c_ToPrecision val mba (fromIntegral ndigits))
        where len = c_ToPrecisionLength
              {-# NOINLINE len #-}

    toShortest :: Double -> BB.Builder
    toShortest = primBounded $ CBB.convert "toShortest" len c_ToShortest
        where len = c_ToShortestLength
              {-# NOINLINE len #-}

    toFixed :: Int -> Double -> BB.Builder
    toFixed ndigits = primBounded $ CBB.convert "toFixed" len $ \val mba ->
                  c_ToFixed val mba (fromIntegral ndigits)
        where len = c_ToFixedLength
              {-# NOINLINE len #-}


instance Convertable Float BB.Builder where
    toExponential :: Int -> Float -> BB.Builder
    toExponential ndigits = primBounded $ CBB.convert "toExponential" len $ \val mba ->
                        c_ToExponentialFloat val mba (fromIntegral ndigits)
        where len = c_ToExponentialLength
              {-# NOINLINE len #-}

    toPrecision :: Int -> Float -> BB.Builder
    toPrecision ndigits = primBounded (CBB.convert "toPrecision" len $ \val mba ->
                      c_ToPrecisionFloat val mba (fromIntegral ndigits))
        where len = c_ToPrecisionLength
              {-# NOINLINE len #-}

    toShortest :: Float -> BB.Builder
    toShortest = primBounded $ CBB.convert "toShortest" len c_ToShortestFloat
        where len = c_ToShortestLength
              {-# NOINLINE len #-}

    toFixed :: Int -> Float -> BB.Builder
    toFixed ndigits = primBounded $ CBB.convert "toFixed" len $ \val mba ->
                  c_ToFixedFloat val mba (fromIntegral ndigits)
        where len = c_ToFixedLength
              {-# NOINLINE len #-}

-- Fast conversion to bytestring.
-- Although about 15 times faster than plain 'show', these functions
-- are /slower/ than their 'Text' counterparts, at roughly half the
-- speed.  (This seems to be due to the cost of allocating
-- 'ByteString' values via @malloc@.)

instance Convertable Double B.ByteString where
    toExponential :: Int -> Double -> B.ByteString
    toExponential ndigits = CB.convert "toExponential" len $ \val mba ->
                        c_ToExponential val mba (fromIntegral ndigits)
        where len = c_ToExponentialLength
              {-# NOINLINE len #-}

    toFixed :: Int -> Double -> B.ByteString
    toFixed ndigits = CB.convert "toFixed" len $ \val mba ->
                  c_ToFixed val mba (fromIntegral ndigits)
        where len = c_ToFixedLength
              {-# NOINLINE len #-}

    toShortest :: Double -> B.ByteString
    toShortest = CB.convert "toShortest" len c_ToShortest
        where len = c_ToShortestLength
              {-# NOINLINE len #-}

    toPrecision :: Int -> Double -> B.ByteString
    toPrecision ndigits = CB.convert "toPrecision" len $ \val mba ->
                      c_ToPrecision val mba (fromIntegral ndigits)
        where len = c_ToPrecisionLength
              {-# NOINLINE len #-}


instance Convertable Float B.ByteString where
    toExponential :: Int -> Float -> B.ByteString
    toExponential ndigits = CB.convert "toExponential" len $ \val mba ->
                        c_ToExponentialFloat val mba (fromIntegral ndigits)
        where len = c_ToExponentialLength
              {-# NOINLINE len #-}

    toFixed :: Int -> Float -> B.ByteString
    toFixed ndigits = CB.convert "toFixed" len $ \val mba ->
                  c_ToFixedFloat val mba (fromIntegral ndigits)
        where len = c_ToFixedLength
              {-# NOINLINE len #-}

    toShortest :: Float -> B.ByteString
    toShortest = CB.convert "toShortest" len c_ToShortestFloat
        where len = c_ToShortestLength
              {-# NOINLINE len #-}

    toPrecision :: Int -> Float -> B.ByteString
    toPrecision ndigits = CB.convert "toPrecision" len $ \val mba ->
                      c_ToPrecisionFloat val mba (fromIntegral ndigits)
        where len = c_ToPrecisionLength
              {-# NOINLINE len #-}


instance Convertable Double Text where
    toExponential :: Int -> Double -> Text
    toExponential ndigits = CT.convert "toExponential" len $ \val mba ->
                            c_Text_ToExponential val mba (fromIntegral ndigits)
        where len = c_ToExponentialLength
              {-# NOINLINE len #-}

    toFixed :: Int -> Double -> Text
    toFixed ndigits = CT.convert "toFixed" len $ \val mba ->
                    c_Text_ToFixed val mba (fromIntegral ndigits)
        where len = c_ToFixedLength
              {-# NOINLINE len #-}

    toShortest :: Double -> Text
    toShortest = CT.convert "toShortest" len c_Text_ToShortest
        where len = c_ToShortestLength
              {-# NOINLINE len #-}

    toPrecision :: Int -> Double -> Text
    toPrecision ndigits = CT.convert "toPrecision" len $ \val mba ->
                          c_Text_ToPrecision val mba (fromIntegral ndigits)
        where len = c_ToPrecisionLength
              {-# NOINLINE len #-}


instance Convertable Float Text where
    toExponential :: Int -> Float -> Text
    toExponential ndigits = CT.convert "toExponential" len $ \val mba ->
                            c_Text_ToExponentialFloat val mba (fromIntegral ndigits)
        where len = c_ToExponentialLength
              {-# NOINLINE len #-}

    toFixed :: Int -> Float -> Text
    toFixed ndigits = CT.convert "toFixed" len $ \val mba ->
                    c_Text_ToFixedFloat val mba (fromIntegral ndigits)
        where len = c_ToFixedLength
              {-# NOINLINE len #-}

    toShortest :: Float -> Text
    toShortest = CT.convert "toShortest" len c_Text_ToShortestFloat
        where len = c_ToShortestLength
              {-# NOINLINE len #-}

    toPrecision :: Int -> Float -> Text
    toPrecision ndigits = CT.convert "toPrecision" len $ \val mba ->
                          c_Text_ToPrecisionFloat val mba (fromIntegral ndigits)
        where len = c_ToPrecisionLength
              {-# NOINLINE len #-}


instance Convertable Double T.Builder where
    toExponential :: Int -> Double -> T.Builder
    toExponential ndigits = CTB.convert "toExponential" len $ \val mba ->
                            c_Text_ToExponential val mba (fromIntegral ndigits)
        where len = c_ToExponentialLength
              {-# NOINLINE len #-}

    toFixed :: Int -> Double -> T.Builder
    toFixed ndigits = CTB.convert "toFixed" len $ \val mba ->
                      c_Text_ToFixed val mba (fromIntegral ndigits)
        where len = c_ToFixedLength
              {-# NOINLINE len #-}

    toShortest :: Double -> T.Builder
    toShortest = CTB.convert "toShortest" len c_Text_ToShortest
        where len = c_ToShortestLength
              {-# NOINLINE len #-}

    toPrecision :: Int -> Double -> T.Builder
    toPrecision ndigits = CTB.convert "toPrecision" len $ \val mba ->
                          c_Text_ToPrecision val mba (fromIntegral ndigits)
        where len = c_ToPrecisionLength
              {-# NOINLINE len #-}


instance Convertable Float T.Builder where
    toExponential :: Int -> Float -> T.Builder
    toExponential ndigits = CTB.convert "toExponential" len $ \val mba ->
                            c_Text_ToExponentialFloat val mba (fromIntegral ndigits)
        where len = c_ToExponentialLength
              {-# NOINLINE len #-}

    toFixed :: Int -> Float -> T.Builder
    toFixed ndigits = CTB.convert "toFixed" len $ \val mba ->
                      c_Text_ToFixedFloat val mba (fromIntegral ndigits)
        where len = c_ToFixedLength
              {-# NOINLINE len #-}

    toShortest :: Float -> T.Builder
    toShortest = CTB.convert "toShortest" len c_Text_ToShortestFloat
        where len = c_ToShortestLength
              {-# NOINLINE len #-}

    toPrecision :: Int -> Float -> T.Builder
    toPrecision ndigits = CTB.convert "toPrecision" len $ \val mba ->
                          c_Text_ToPrecisionFloat val mba (fromIntegral ndigits)
        where len = c_ToPrecisionLength
              {-# NOINLINE len #-}
