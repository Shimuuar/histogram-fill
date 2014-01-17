import Test.Tasty            (testGroup,defaultMain)
import qualified Test.Histogram

main :: IO ()
main =
  defaultMain $ testGroup "tests"
    [ Test.Histogram.tests
    ]
