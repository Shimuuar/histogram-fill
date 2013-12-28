{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import System.Random.MWC
import Criterion.Main

import Data.Histogram.Fill
import Data.Histogram


main :: IO ()
main =
  defaultMain
  [ bgroup "bin"
    [ bench "BinI"       $ nf (toIndex (binI0 10))      4
    , bench "BinInt"     $ nf (toIndex (BinInt 4 4 4))  12
    , bench "BinD"       $ nf (toIndex (binD 0 10 1))   0.33
    , bench "BinF::F"    $ nf (toIndex (binF 0 10 1))  (0.33 :: Float)
    , bench "BinF::D"    $ nf (toIndex (binF 0 10 1))  (0.33 :: Double)
    , bench "BinI><BinI" $ nf (toIndex (binI0 10    >< binI0 10   )) (3,5)
    , bench "BinD><BinD" $ nf (toIndex (binD 0 10 1 >< binD 0 10 1)) (0.3,0.5)
    ]
  , bgroup "hist"
    [ bench "BinI-2"    $ nf (fillBuilderVec (forceInt    -<< mkSimple   (binI0 100))) (mkRange  100)
    , bench "BinI-3"    $ nf (fillBuilderVec (forceInt    -<< mkSimple   (binI0 100))) (mkRange  1000)
    , bench "BinI-4"    $ nf (fillBuilderVec (forceInt    -<< mkSimple   (binI0 100))) (mkRange  10000)
    , bench "BinI-w2"   $ nf (fillBuilderVec (forceDouble -<< mkWeighted (binI0 100))) (mkRangeW 100)
    , bench "BinI-w3"   $ nf (fillBuilderVec (forceDouble -<< mkWeighted (binI0 100))) (mkRangeW 1000)
    , bench "BinI-w4"   $ nf (fillBuilderVec (forceDouble -<< mkWeighted (binI0 100))) (mkRangeW 10000)
    ]
  ]

mkRange n = runST $ do
  gen <- create
  U.replicateM n (uniform gen)

mkRangeW n = runST $ do
  gen <- create
  U.replicateM n $ liftM2 (,) (uniformR (-10,110) gen) (uniformR (0,10) gen)
