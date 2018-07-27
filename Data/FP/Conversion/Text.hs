{-# LANGUAGE CPP, MagicHash, Rank2Types #-}

-- |
-- Module      : Data.FP.Conversion.Text
-- Copyright   : (c) 2011 MailRank, Inc. , 2018 Rinat Stryungis 
--
-- License     : BSD-style
-- Maintainer  : lazybonesxp@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, efficient support for converting between double precision
-- floating point values and text.
--
-- These functions are about 30 times faster than the default 'show'
-- implementation for the 'Double' type.

module Data.FP.Conversion.Text
    (
      convert
    , convertFloat
    ) where

import Control.Monad (when)
#if MIN_VERSION_base(4,4,0)
import Control.Monad.ST.Unsafe (unsafeIOToST)
#else
import Control.Monad.ST (unsafeIOToST)
#endif
import Control.Monad.ST (runST)
import Data.Text.Internal (Text(Text))
import Foreign.C.Types (CFloat, CDouble, CInt)
import GHC.Prim (MutableByteArray#)
import qualified Data.Text.Array as A


convert :: String -> CInt
        -> (forall s. CDouble -> MutableByteArray# s -> IO CInt)
        -> Double -> Text
convert func len act val = runST go
  where
    go = do
      buf <- A.new (fromIntegral len)
      size <- unsafeIOToST $ act (realToFrac val) (A.maBA buf)
      when (size == -1) .
        fail $ "Data.Double.Conversion.Text." ++ func ++
               ": conversion failed (invalid precision requested)"
      frozen <- A.unsafeFreeze buf
      return $ Text frozen 0 (fromIntegral size)

convertFloat :: String -> CInt
        -> (forall s. CFloat -> MutableByteArray# s -> IO CInt)
        -> Float -> Text
convertFloat func len act val = runST go
  where
    go = do
      buf <- A.new (fromIntegral len)
      size <- unsafeIOToST $ act (realToFrac val) (A.maBA buf)
      when (size == -1) .
        fail $ "Data.Double.Conversion.Text." ++ func ++
               ": conversion failed (invalid precision requested)"
      frozen <- A.unsafeFreeze buf
      return $ Text frozen 0 (fromIntegral size)