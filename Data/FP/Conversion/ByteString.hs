-- |
-- Module      : Data.FP.Conversion.ByteString
-- Copyright   : (c) 2011 MailRank, Inc., Rinat Stryungis
--
-- License     : BSD-style
-- Maintainer  : lazybonesxp@gmail.com
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

module Data.FP.Conversion.ByteString
    (
      convert
    , convertFloat
    ) where

import Control.Monad (when)
import Foreign.ForeignPtr (withForeignPtr)
import Data.Word (Word8)
import Data.ByteString.Internal (ByteString(..), mallocByteString)
import Foreign.C.Types (CFloat, CDouble, CInt)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)


convert :: String -> CInt -> (CDouble -> Ptr Word8 -> IO CInt)
        -> Double -> ByteString
convert func len act val = unsafePerformIO $ do
  fp <- mallocByteString (fromIntegral len)
  size <- withForeignPtr fp $ act (realToFrac val)
  when (size == -1) .
    fail $ "Data.Double.Conversion.ByteString." ++ func ++
           ": conversion failed (invalid precision requested)"
  return $ PS fp 0 (fromIntegral size)

convertFloat :: String -> CInt -> (CFloat -> Ptr Word8 -> IO CInt)
        -> Float -> ByteString
convertFloat func len act val = unsafePerformIO $ do
  fp <- mallocByteString (fromIntegral len)
  size <- withForeignPtr fp $ act (realToFrac val)
  when (size == -1) .
    fail $ "Data.Double.Conversion.ByteString." ++ func ++
           ": conversion failed (invalid precision requested)"
  return $ PS fp 0 (fromIntegral size)