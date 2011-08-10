import Data.Double.Conversion.Text
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.Text as T

shortest a = (read . T.unpack . toShortest) a == a

main = defaultMain [
        testProperty "shortest" shortest
       ]
