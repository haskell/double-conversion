{-# LANGUAGE CPP, MagicHash, Rank2Types #-}
-- |
-- Module      : Data.FP.Conversion.TextBuilder
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

module Data.FP.Conversion.TextBuilder
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
import Data.Text.Internal.Builder (Builder, writeN)
import Foreign.C.Types (CFloat, CDouble, CInt)
import GHC.Prim (MutableByteArray#)
import qualified Data.Text.Array as A

convert :: String -> CInt
        -> (forall s. CDouble -> MutableByteArray# s -> IO CInt)
        -> Double -> Builder
convert func len act val = writeN (fromIntegral len) $ \(A.MArray maBa) _ -> do 
    size <- unsafeIOToST $ act (realToFrac val) maBa
    when (size == -1) .
        fail $ "Data.Double.Conversion.Text." ++ func ++
               ": conversion failed (invalid precision requested)"
    return ()

convertFloat :: String -> CInt
        -> (forall s. CFloat -> MutableByteArray# s -> IO CInt)
        -> Float -> Builder
convertFloat func len act val = writeN (fromIntegral len) $ \(A.MArray maBa) _ -> do 
    size <- unsafeIOToST $ act (realToFrac val) maBa
    when (size == -1) .
        fail $ "Data.Double.Conversion.Text." ++ func ++
               ": conversion failed (invalid precision requested)"
    return ()