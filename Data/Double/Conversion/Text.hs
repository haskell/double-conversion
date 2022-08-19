{-# LANGUAGE CPP, MagicHash, Rank2Types #-}

-- |
-- Module      : Data.Double.Conversion.Text
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module left now only for compatibility and should not be used
-- in new projects. 
-- Please, use Convertable type class from Data.Double.Conversion.Convertable
--
-- It is espesially recommended to convert a large amount of numbers via text builder
-- using methods of Convertable type class. It is about 10-15x faster.
-- 
-- Fast, efficient support for converting between double precision
-- floating point values and text.
--
-- These functions are about 30 times faster than the default 'show'
-- implementation for the 'Double' type.

module Data.Double.Conversion.Text
    (
      toExponential
    , toFixed
    , toPrecision
    , toShortest
    ) where

import qualified Data.Double.Conversion.Convertable
import Data.Text.Internal (Text)

-- | Compute a representation in exponential format with the requested
-- number of digits after the decimal point. The last emitted digit is
-- rounded.  If -1 digits are requested, then the shortest exponential
-- representation is computed.
toExponential :: Int -> Double -> Text
toExponential = Data.Double.Conversion.Convertable.toExponential

-- | Compute a decimal representation with a fixed number of digits
-- after the decimal point. The last emitted digit is rounded.
toFixed :: Int -> Double -> Text
toFixed = Data.Double.Conversion.Convertable.toFixed

-- | Compute the shortest string of digits that correctly represent
-- the input number.
toShortest :: Double -> Text
toShortest = Data.Double.Conversion.Convertable.toShortest

-- | Compute @precision@ leading digits of the given value either in
-- exponential or decimal format. The last computed digit is rounded.
toPrecision :: Int -> Double -> Text
toPrecision = Data.Double.Conversion.Convertable.toPrecision