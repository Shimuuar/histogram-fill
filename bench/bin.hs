{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Applicative
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
    [ bench "BinI-2"    $ nf (fill (forceInt    -<< mkSimple   (binI0 100))) (mkRange  100)
    , bench "BinI-3"    $ nf (fill (forceInt    -<< mkSimple   (binI0 100))) (mkRange  1000)
    , bench "BinI-4"    $ nf (fill (forceInt    -<< mkSimple   (binI0 100))) (mkRange  10000)
    , bench "BinI-w2"   $ nf (fill (forceDouble -<< mkWeighted (binI0 100))) (mkRangeW 100)
    , bench "BinI-w3"   $ nf (fill (forceDouble -<< mkWeighted (binI0 100))) (mkRangeW 1000)
    , bench "BinI-w4"   $ nf (fill (forceDouble -<< mkWeighted (binI0 100))) (mkRangeW 10000)
    ]
  ]

-- Fill histogram from vector
fill :: (U.Unbox a, Bin bin, Num val)
      => HBuilder a (Histogram bin val) -> U.Vector a -> Histogram bin val
fill hb vec = runST $ do
  h <- toHBuilderST hb
  U.mapM_ (feedOne h) vec
  freezeHBuilderM h
{-# INLINE fill #-}

mkRange n = runST $ do
  gen <- create
  U.replicateM n (uniformR (-10,110) g)

mkRangeW n = runST $ do
  gen <- create
  U.replicateM n ((,) <$> uniformR (-10,110) g <*> uniformR (0,10))
