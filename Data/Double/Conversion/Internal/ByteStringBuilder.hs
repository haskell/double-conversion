{-# LANGUAGE CPP, TypeFamilies, TypeOperators #-}

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

module Data.Double.Conversion.Internal.ByteStringBuilder
    (convert
    ) where

import Control.Monad (when)

#if MIN_VERSION_bytestring(0,10,12)
import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boundedPrim)
#else
import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boudedPrim)
#endif

import Data.Double.Conversion.Internal.FFI (ForeignFloating)
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CFloat, CInt)
import Foreign.Ptr (Ptr, plusPtr)

convert :: (RealFloat a, RealFloat b , b ~ ForeignFloating a) => String -> CInt -> (b -> Ptr Word8 -> IO CInt) -> BoundedPrim a
{-# SPECIALIZE convert :: String -> CInt -> (CDouble -> Ptr Word8 -> IO CInt) -> BoundedPrim Double #-}
{-# SPECIALIZE convert :: String -> CInt -> (CFloat -> Ptr Word8 -> IO CInt) -> BoundedPrim Float #-}
{-# INLINABLE convert #-}
#if MIN_VERSION_bytestring(0,10,12)
convert func len act = boundedPrim (fromIntegral len) $ \val ptr -> do
#else
convert func len act = boudedPrim (fromIntegral len) $ \val ptr -> do
#endif
  size <- act (realToFrac val) ptr
  when (size == -1) .
    fail $ "Data.Double.Conversion.ByteString." ++ func ++
           ": conversion failed (invalid precision requested)"
  return (ptr `plusPtr` (fromIntegral size))
