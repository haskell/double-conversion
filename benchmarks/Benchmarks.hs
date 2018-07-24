{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

import Criterion.Main
import Data.List
import qualified Data.Double.Conversion.ByteString as B
import qualified Data.Double.Conversion.ByteStringBuilder as BL
import qualified Data.Double.Conversion.Text as T
import qualified Data.Double.Conversion.TextBuilder as CBT 
import Foreign.C.Types (CInt(CInt), CDouble(CDouble))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal.Builder as BT 
import qualified Text.Show.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS 
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE  

testList = [2.08345919 .. 20002.08345919] :: [Double]

testListBSBuilder :: (Double -> BB.Builder) -> [Double] -> BSS.ByteString
testListBSBuilder func list = BSL.toStrict $ BBE.toLazyByteStringWith (BBE.safeStrategy 128 128) BSL.empty $ foldr (\x y -> (func x) <> y) mempty list

testListByteString :: (Double -> BSS.ByteString) -> [Double] -> BSS.ByteString
testListByteString func = foldr (\x y -> (func x) <> y) mempty

testListText :: (Double -> T.Text) -> [Double] -> T.Text 
testListText func = foldr (\x y -> (func x) <> y) mempty

testListTextBuilder :: (Double -> BT.Builder) -> [Double] -> T.Text 
testListTextBuilder func list = TL.toStrict $ BT.toLazyText $ foldr (\x y -> (func x) <> y) mempty list

testFuncBSD :: [Double] -> BSL.ByteString
testFuncBSD = \list -> BB.toLazyByteString $ foldr (\x y -> (BB.doubleDec x) <> y) mempty list

main = defaultMain [
         bgroup "haskell-single" [
           bench "show" $ nf show (pi::Double)
         , bench "bytestring-show" $ whnf BS.show (pi::Double)
         , bgroup "text" [
             bench "toShortest" $ whnf T.toShortest pi
           , bench "toExponential" $ whnf (T.toExponential 3) pi
           , bench "toPrecision" $ whnf (T.toExponential 8) pi
           , bench "toFixed" $ whnf (T.toFixed 8) pi
           ]
         , bgroup "bytestring" [
             bench "toShortest" $ whnf B.toShortest pi
           , bench "toExponential" $ whnf (B.toExponential 3) pi
           , bench "toPrecision" $ whnf (B.toExponential 8) pi
           , bench "toFixed" $ whnf (B.toFixed 8) pi
           ]
         , bgroup "bytestringBuilder" [
             bench "toShortest" $ nf (BB.toLazyByteString . BL.toShortest) pi 
           , bench "toPrecision" $ nf (BB.toLazyByteString . (BL.toExponential 8)) pi  
           , bench "toPrecision" $ nf (BB.toLazyByteString . (BL.toPrecision 8)) pi
           , bench "toFixed" $ nf (BB.toLazyByteString . (BL.toFixed 8)) pi
           ]
         ]
       , bgroup "Haskell-list" [
           bgroup "bytestring" [
             bench "toShortest" $ nf (testListByteString B.toShortest) testList
           , bench "toExponential" $ nf (testListByteString $ B.toExponential 8) testList 
           , bench "toPrecision" $ nf (testListByteString $ B.toPrecision 8) testList
           , bench "toFixed" $ nf (testListByteString $ B.toFixed 8) testList]
        ,  bgroup "bytestring-builder" [
             bench "toShortest" $ nf (testListBSBuilder BL.toShortest) testList
           , bench "toExponential" $ nf (testListBSBuilder $ BL.toExponential 8) testList
           , bench "toPrecision" $ nf (testListBSBuilder $ BL.toPrecision 8) testList
           , bench "toFixed" $ nf (testListBSBuilder $ BL.toFixed 8) testList]
        ,  bgroup "text" [
             bench "toShortest" $ nf (testListText T.toShortest) testList
           , bench "toExponential" $ nf (testListText $ T.toExponential 8) testList  
           , bench "toPrecision" $ nf (testListText $ T.toPrecision 8) testList  
           , bench "toFixed" $ nf (testListText $ T.toFixed 8) testList ]
        ,  bgroup "text-builder" [
             bench "toShortest" $ nf (testListTextBuilder CBT.toShortest) testList
           , bench "toExponential" $ nf (testListTextBuilder $ CBT.toExponential 8) testList 
           , bench "toPrecision" $ nf (testListTextBuilder$ CBT.toPrecision 8) testList 
           , bench "toFixed" $ nf (testListTextBuilder $ CBT.toFixed 8) testList ]   
        ,  bgroup "bytestring-builder-default" [
             bench "toFixed" $ nf testFuncBSD testList]
         ]
       , bgroup "sprintf" [
           bench "exact" $ whnf sprintf_exact pi
         , bench "exponential" $ whnf (sprintf_exponential 3) pi
         , bench "fixed" $ whnf (sprintf_fixed 8) pi
         , bench "generic" $ whnf (sprintf_generic 6) pi
         , bench "generic_default" $ whnf sprintf_generic_default pi
         ]
       ]

foreign import ccall unsafe sprintf_exact :: CDouble -> ()
foreign import ccall unsafe sprintf_exponential :: CInt -> CDouble -> ()
foreign import ccall unsafe sprintf_fixed :: CInt -> CDouble -> ()
foreign import ccall unsafe sprintf_generic :: CInt -> CDouble -> ()
foreign import ccall unsafe sprintf_generic_default :: CDouble -> ()
