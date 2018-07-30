{-# LANGUAGE MultiParamTypeClasses, DefaultSignatures #-}

-- |
-- Module      : Data.FP.Conversion.ByteStringBuilder
-- Copyright   : (c) 2011 MailRank, Inc. , 2018 Rinat Stryungis 
--
-- License     : BSD-style
-- Maintainer  : lazybonesxp@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, efficient support for converting between double precision
-- floating point values and bytestring builder.

-- This functions are much slower on the single value, but also it is much faster in conversting big set of 
-- numbers, than bytestring functions. See benchmark. 

module Data.FP.Conversion.ByteStringBuilder
    (convert,
     convertFloat
    ) where 

import Control.Monad (when)

import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boudedPrim)

import Data.Word (Word8)
import Foreign.C.Types (CFloat, CDouble, CInt)
import Foreign.Ptr (Ptr, plusPtr)

convert :: String -> CInt -> (CDouble -> Ptr Word8 -> IO CInt) -> BoundedPrim Double
convert func len act = boudedPrim (fromIntegral len) $ \val ptr -> do 
  size <- act (realToFrac val) ptr
  when (size == -1) .
    fail $ "Data.Double.Conversion.ByteString." ++ func ++
           ": conversion failed (invalid precision requested)"      
  return (ptr `plusPtr` (fromIntegral size))

convertFloat :: String -> CInt -> (CFloat -> Ptr Word8 -> IO CInt) -> BoundedPrim Float
convertFloat func len act = boudedPrim (fromIntegral len) $ \val ptr -> do 
  size <- act (realToFrac val) ptr
  when (size == -1) .
    fail $ "Data.Double.Conversion.ByteString." ++ func ++
           ": conversion failed (invalid precision requested)"
  return (ptr `plusPtr` (fromIntegral size))