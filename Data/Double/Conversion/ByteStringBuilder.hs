{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Double.Conversion.ByteStringBuilder
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, efficient support for converting between double precision
-- floating point values and bytestring builder.

-- This functions are much slower on the single value, but also it is much faster in conversting big set of
-- numbers, than bytestring functions. See benchmark.

module Data.Double.Conversion.ByteStringBuilder
    (convert
    ) where

import Control.Monad (when)

import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boudedPrim)

import Data.Double.Conversion.FFI (ForeignFloating)
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CFloat, CInt)
import Foreign.Ptr (Ptr, plusPtr)

convert :: (RealFloat a, RealFloat b , b ~ ForeignFloating a) => String -> CInt -> (b -> Ptr Word8 -> IO CInt) -> BoundedPrim a
{-# SPECIALIZE convert :: String -> CInt -> (CDouble -> Ptr Word8 -> IO CInt) -> BoundedPrim Double #-}
{-# SPECIALIZE convert :: String -> CInt -> (CFloat -> Ptr Word8 -> IO CInt) -> BoundedPrim Float #-}
{-# INLINABLE convert #-}
convert func len act = boudedPrim (fromIntegral len) $ \val ptr -> do
  size <- act (realToFrac val) ptr
  when (size == -1) .
    fail $ "Data.Double.Conversion.ByteString." ++ func ++
           ": conversion failed (invalid precision requested)"
  return (ptr `plusPtr` (fromIntegral size))
