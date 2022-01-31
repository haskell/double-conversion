{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

import Criterion.Main
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Double.Conversion.Convertable as BL
import Data.List (foldr)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Internal.Builder as BT
import qualified Data.Text.Lazy as TL
import Foreign.C.Types (CDouble(CDouble), CInt(CInt))
import qualified Text.Show.ByteString as BS

testList = [2.08345919 .. 20002.08345919] :: [Double]
testFloatList = [2.08345919 .. 20002.08345919] :: [Float]

testListBSBuilder :: (Double -> BB.Builder) -> [Double] -> BSS.ByteString
testListBSBuilder func list = BSL.toStrict $ BBE.toLazyByteStringWith (BBE.safeStrategy 128 128) BSL.empty $ foldr (\x y -> (func x) <> y) mempty list

testListBSBuilderFloat :: (Float -> BB.Builder) -> [Float] -> BSS.ByteString
testListBSBuilderFloat func list = BSL.toStrict $ BBE.toLazyByteStringWith (BBE.safeStrategy 128 128) BSL.empty $ foldr (\x y -> (func x) <> y) mempty list

testListByteString :: (Double -> BSS.ByteString) -> [Double] -> BSS.ByteString
testListByteString func = foldr (\x y -> (func x) <> y) mempty

testListByteStringFloat :: (Float -> BSS.ByteString) -> [Float] -> BSS.ByteString
testListByteStringFloat func = foldr (\x y -> (func x) <> y) mempty

testListText :: (Double -> T.Text) -> [Double] -> T.Text
testListText func = foldr (\x y -> (func x) <> y) mempty

testListTextFloat :: (Float -> T.Text) -> [Float] -> T.Text
testListTextFloat func = foldr (\x y -> (func x) <> y) mempty

testListTextBuilder :: (Double -> BT.Builder) -> [Double] -> T.Text
testListTextBuilder func list = TL.toStrict $ BT.toLazyText $ foldr (\x y -> (func x) <> y) mempty list

testListTextBuilderFloat :: (Float -> BT.Builder) -> [Float] -> T.Text
testListTextBuilderFloat func list = TL.toStrict $ BT.toLazyText $ foldr (\x y -> (func x) <> y) mempty list

testFuncBuilderDefault :: [Double] -> BSS.ByteString
testFuncBuilderDefault = \list -> BSL.toStrict $ BB.toLazyByteString $ foldr (\x y -> (BB.doubleDec x) <> y) mempty list

testFuncBuilderDefaultFloat :: [Float] -> BSS.ByteString
testFuncBuilderDefaultFloat = \list -> BSL.toStrict $ BB.toLazyByteString $ foldr (\x y -> (BB.floatDec x) <> y) mempty list


main = defaultMain [
         bgroup "haskell-single" [
           bench "show" $ nf show (pi::Double)
         , bench "bytestring-show" $ whnf BS.show (pi::Double)
         , bgroup "text" [
             bench "toShortest" $ whnf (BL.toShortest :: Double -> T.Text) pi
           , bench "toExponential" $ whnf ((BL.toExponential 3) :: Double -> T.Text) pi
           , bench "toPrecision" $ whnf ((BL.toExponential 8) :: Double -> T.Text) pi
           , bench "toFixed" $ whnf ((BL.toFixed 8) :: Double -> T.Text) pi
           ]
         , bgroup "bytestring" [
             bench "toShortest" $ whnf (BL.toShortest :: Double -> BSS.ByteString) pi
           , bench "toExponential" $ whnf ((BL.toExponential 3) :: Double -> BSS.ByteString) pi
           , bench "toPrecision" $ whnf ((BL.toExponential 8) :: Double -> BSS.ByteString) pi
           , bench "toFixed" $ whnf ((BL.toFixed 8) :: Double -> BSS.ByteString) pi
           ]
         , bgroup "bytestringBuilder" [
             bench "toShortest" $ nf (BB.toLazyByteString . BL.toShortest) (pi::Double)
           , bench "toPrecision" $ nf (BB.toLazyByteString . (BL.toExponential 8)) (pi::Double)
           , bench "toPrecision" $ nf (BB.toLazyByteString . (BL.toPrecision 8)) (pi::Double)
           , bench "toFixed" $ nf (BB.toLazyByteString . (BL.toFixed 8)) (pi::Double)
           ]
         ]
       , bgroup "Haskell-list" [
           bgroup "bytestring" [
             bench "toShortest" $ nf (testListByteString BL.toShortest) testList
           , bench "toExponential" $ nf (testListByteString $ BL.toExponential 8) testList
           , bench "toPrecision" $ nf (testListByteString $ BL.toPrecision 8) testList
           , bench "toFixed" $ nf (testListByteString $ BL.toFixed 8) testList]
       , bgroup "bytestring-float" [
             bench "toShortest" $ nf (testListByteStringFloat BL.toShortest) testFloatList
           , bench "toExponential" $ nf (testListByteStringFloat $ BL.toExponential 8) testFloatList
           , bench "toPrecision" $ nf (testListByteStringFloat $ BL.toPrecision 8) testFloatList
           , bench "toFixed" $ nf (testListByteStringFloat $ BL.toFixed 8) testFloatList]
        ,  bgroup "bytestring-builder" [
             bench "toShortest" $ nf (testListBSBuilder BL.toShortest) testList
           , bench "toExponential" $ nf (testListBSBuilder $ BL.toExponential 8) testList
           , bench "toPrecision" $ nf (testListBSBuilder $ BL.toPrecision 8) testList
           , bench "toFixed" $ nf (testListBSBuilder $ BL.toFixed 8) testList ]
        ,  bgroup "bytestring-builder-float" [
             bench "toShortest" $ nf (testListBSBuilderFloat BL.toShortest) testFloatList
           , bench "toExponential" $ nf (testListBSBuilderFloat $ BL.toExponential 8) testFloatList
           , bench "toPrecision" $ nf (testListBSBuilderFloat $ BL.toPrecision 8) testFloatList
           , bench "toFixed" $ nf (testListBSBuilderFloat $ BL.toFixed 8) testFloatList ]
        ,  bgroup "text" [
             bench "toShortest" $ nf (testListText BL.toShortest) testList
           , bench "toExponential" $ nf (testListText $ BL.toExponential 8) testList
           , bench "toPrecision" $ nf (testListText $ BL.toPrecision 8) testList
           , bench "toFixed" $ nf (testListText $ BL.toFixed 8) testList ]
        ,  bgroup "text-float" [
             bench "toShortest" $ nf (testListTextFloat BL.toShortest) testFloatList
           , bench "toExponential" $ nf (testListTextFloat $ BL.toExponential 8) testFloatList
           , bench "toPrecision" $ nf (testListTextFloat $ BL.toPrecision 8) testFloatList
           , bench "toFixed" $ nf (testListTextFloat $ BL.toFixed 8) testFloatList ]
        ,  bgroup "text-builder" [
             bench "toShortest" $ nf (testListTextBuilder BL.toShortest) testList
           , bench "toExponential" $ nf (testListTextBuilder $ BL.toExponential 8) testList
           , bench "toPrecision" $ nf (testListTextBuilder$ BL.toPrecision 8) testList
           , bench "toFixed" $ nf (testListTextBuilder $ BL.toFixed 8) testList ]
        ,  bgroup "text-builder-float" [
             bench "toShortest" $ nf (testListTextBuilderFloat BL.toShortest) testFloatList
           , bench "toExponential" $ nf (testListTextBuilderFloat $ BL.toExponential 8) testFloatList
           , bench "toPrecision" $ nf (testListTextBuilderFloat $ BL.toPrecision 8) testFloatList
           , bench "toFixed" $ nf (testListTextBuilderFloat $ BL.toFixed 8) testFloatList ]
        ,  bgroup "bytestring-builder-default" [
             bench "Double" $ nf (testFuncBuilderDefault) testList
        ,    bench "Float" $ nf (testFuncBuilderDefaultFloat) testFloatList ]
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
