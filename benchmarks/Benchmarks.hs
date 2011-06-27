{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

import Criterion.Main
import Data.Double.Conversion
import Foreign.C.Types (CInt, CDouble)
import qualified Data.Text as T

showText :: Double -> T.Text
showText d = T.pack (show d)

main = defaultMain [
         bgroup "haskell" [
           bench "show" $ whnf showText pi
         , bench "toShortest" $ whnf toShortest pi
         , bench "toExponential" $ whnf (toExponential 3) pi
         , bench "toPrecision" $ whnf (toExponential 8) pi
         , bench "toFixed" $ whnf (toFixed 8) pi
         ]
       , bgroup "sprintf" [
           bench "exponential" $ whnf (sprintf_exponential 3) pi
         , bench "fixed" $ whnf (sprintf_fixed 8) pi
         , bench "generic" $ whnf (sprintf_generic 6) pi
         , bench "generic_default" $ whnf (sprintf_generic_default 6) pi
         ]
       ]

foreign import ccall safe sprintf_exponential :: CInt -> CDouble -> ()
foreign import ccall safe sprintf_fixed :: CInt -> CDouble -> ()
foreign import ccall safe sprintf_generic :: CInt -> CDouble -> ()
foreign import ccall safe sprintf_generic_default :: CInt -> CDouble -> ()
