import Test.Tasty            (testGroup,defaultMain)
import qualified Test.Cereal
import qualified Test.Histogram

main :: IO ()
main =
  defaultMain $ testGroup "tests"
    [ Test.Histogram.tests
    , Test.Cereal.tests
    ]
