{-# LANGUAGE CPP, MagicHash, Rank2Types, TypeFamilies, TypeOperators #-}
-- |
-- Module      : Data.Double.Conversion.TextBuilder
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

module Data.Double.Conversion.Internal.TextBuilder
    (
      convert
    ) where

import Control.Monad (when)
#if MIN_VERSION_base(4,4,0)
import Control.Monad.ST.Unsafe (unsafeIOToST)
#else
import Control.Monad.ST (unsafeIOToST)
#endif
import Data.Double.Conversion.Internal.FFI (ForeignFloating)
import qualified Data.Text.Array as A
import Data.Text.Internal.Builder (Builder, writeN)
import Foreign.C.Types (CDouble, CFloat, CInt)
import GHC.Prim (MutableByteArray#)
import Control.Monad.ST (runST)

convert :: (RealFloat a, RealFloat b, b ~ ForeignFloating a) => String -> CInt
        -> (forall s. b -> MutableByteArray# s -> IO CInt)
        -> a -> Builder
{-# SPECIALIZE convert :: String -> CInt -> (forall s. CDouble -> MutableByteArray# s -> IO CInt) -> Double -> Builder #-}
{-# SPECIALIZE convert :: String -> CInt -> (forall s. CFloat -> MutableByteArray# s -> IO CInt) -> Float -> Builder #-}
{-# INLINABLE convert #-}
convert func len act val = runST $ do 
#if MIN_VERSION_text(2,0,0)
  mTempArr@(A.MutableByteArray tempMArr) <- A.new (fromIntegral len)
#else
  mTempArr@(A.MArray tempMArr) <- A.new (fromIntegral len)
#endif
  size <- unsafeIOToST $ act (realToFrac val) tempMArr
  tempArr <- A.unsafeFreeze mTempArr
  when (size == -1) .
      error $ "Data.Double.Conversion.Text." ++ func ++
              ": conversion failed."
#if MIN_VERSION_text(2,0,0)
  return $ writeN (fromIntegral size) $ \mArr _ -> A.copyI (fromIntegral size) mArr 0 tempArr 0
#else
  return $ writeN (fromIntegral size) $ \mArr _ -> A.copyI mArr 0 tempArr 0 (fromIntegral size)
#endif
