{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Double.Conversion.ByteString
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
-- Although about 15 times faster than plain 'show', these functions
-- are /slower/ than their 'Text' counterparts, at roughly half the
-- speed.  (This seems to be due to the cost of allocating
-- 'ByteString' values via @malloc@.)

module Data.Double.Conversion.ByteString
    ( convert
    ) where

import Control.Monad (when)
import Data.ByteString.Internal (ByteString(..), mallocByteString)
import Data.Double.Conversion.FFI (ForeignFloating)
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CFloat, CInt)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

convert :: (RealFloat a, RealFloat b , b ~ ForeignFloating a) => String -> CInt -> (b -> Ptr Word8 -> IO CInt)
        -> a -> ByteString
{-# SPECIALIZE convert :: String -> CInt -> (CDouble -> Ptr Word8 -> IO CInt) -> Double -> ByteString #-}
{-# SPECIALIZE convert :: String -> CInt -> (CFloat -> Ptr Word8 -> IO CInt) -> Float -> ByteString #-}
{-# INLINABLE convert #-}
convert func len act val = unsafePerformIO $ do
  fp <- mallocByteString (fromIntegral len)
  size <- withForeignPtr fp $ act (realToFrac val)
  when (size == -1) .
    fail $ "Data.Double.Conversion.ByteString." ++ func ++
           ": conversion failed (invalid precision requested)"
  return $ PS fp 0 (fromIntegral size)
