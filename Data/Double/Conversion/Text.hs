{-# LANGUAGE CPP, MagicHash, Rank2Types, TypeFamilies #-}

-- |
-- Module      : Data.Double.Conversion.Text
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, efficient support for converting between double precision
-- floating point values and text.
--
-- These functions are about 30 times faster than the default 'show'
-- implementation for the 'Double' type.

module Data.Double.Conversion.Text
    (
      convert
    ) where

import Control.Monad (when)
#if MIN_VERSION_base(4,4,0)
import Control.Monad.ST.Unsafe (unsafeIOToST)
#else
import Control.Monad.ST (unsafeIOToST)
#endif
import Control.Monad.ST (ST, runST)
import Data.Double.Conversion.FFI (ForeignFloating)
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(Text))
import Foreign.C.Types (CDouble, CFloat, CInt)
import GHC.Prim (MutableByteArray#)


convert :: (RealFloat a, RealFloat b, b ~ ForeignFloating a) => String -> CInt
        -> (forall s. b -> MutableByteArray# s -> IO CInt)
        -> a -> Text
{-# SPECIALIZE convert :: String -> CInt -> (forall s. CDouble -> MutableByteArray# s -> IO CInt) -> Double -> Text #-}
{-# SPECIALIZE convert :: String -> CInt -> (forall s. CFloat -> MutableByteArray# s -> IO CInt) -> Float -> Text #-}
{-# INLINABLE convert #-}
convert func len act val = runST go
  where
    go :: (forall s. ST s Text)
    go = do
      buf <- A.new (fromIntegral len)
      size <- unsafeIOToST $ act (realToFrac val) (A.maBA buf)
      when (size == -1) .
        fail $ "Data.Double.Conversion.Text." ++ func ++
               ": conversion failed (invalid precision requested)"
      frozen <- A.unsafeFreeze buf
      return $ Text frozen 0 (fromIntegral size)
