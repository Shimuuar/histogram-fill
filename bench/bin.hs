import Data.Histogram.Bin
import Criterion.Main

main :: IO ()
main = do
  defaultMain [ bench "BinI"    $ nf (toIndex (binI0 10))     4
              , bench "BinInt"  $ nf (toIndex (BinInt 4 4 4)) 12
              , bench "BinD"    $ nf (toIndex (binD 0 10 1))  0.33
              , bench "BinF::F" $ nf (toIndex (binF 0 10 1))  (0.33 :: Float)
              , bench "BinF::D" $ nf (toIndex (binF 0 10 1))  (0.33 :: Double)
              , bench "BinI><BinI" $ nf (toIndex (binI0 10 >< binI0 10)) (3,5)
              , bench "BinD><BinD" $ nf (toIndex (binD 0 10 1 >< binD 0 10 1)) (0.3,0.5)
              ]