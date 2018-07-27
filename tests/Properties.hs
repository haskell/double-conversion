import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.ByteString.Char8 as B
import qualified Data.FP.Conversion.Convertable as B
import qualified Data.Text as T
import qualified Regressions

shortest :: (Double -> String) -> Double -> Double -> Bool
shortest f a b = case read (f ab) of
                   ba | isNaN ba      -> isNaN ab
                      | isInfinite ba -> isInfinite ab && signum ba == signum ab
                      | otherwise     -> ba == ab
  where ab = a / b

tests :: Test
tests = testGroup "Properties" [
    testProperty "b_shortest" $ shortest (B.unpack . toShortest)
  , testProperty "t_shortest" $ shortest (T.unpack . toShortest)
  ]

main :: IO ()
main = defaultMain [tests, Regressions.tests]
