{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.Double.Conversion
import qualified Data.Text as T

showText :: Double -> T.Text
showText d = T.pack (show d)

main = defaultMain [
         bench "show" $ whnf showText pi
       , bench "toShortest" $ whnf toShortest pi
       , bench "toExponential" $ whnf (toExponential 3) pi
       , bench "toPrecision" $ whnf (toExponential 8) pi
       , bench "toFixed" $ whnf (toFixed 8) pi
       ]
