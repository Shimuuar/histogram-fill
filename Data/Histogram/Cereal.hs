{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Cereal instances for histogram-fill
module Data.Histogram.Cereal (
  ) where

import Control.Applicative
-- import Control.Monad
import Data.Serialize
import qualified Data.Vector.Generic as G

import Data.Histogram.Bin
import Data.Histogram.Generic (Histogram, histogramUO, histData, outOfRange, bins)



----------------------------------------------------------------
-- Bins
----------------------------------------------------------------

instance Serialize BinI where
  get   = binI <$> get <*> get
  put b = put (lowerLimit b) >> put (upperLimit b)

instance (Serialize bX, Serialize bY) => Serialize (Bin2D bX bY) where
  get = Bin2D <$> get <*> get
  put (Bin2D bx by) = put bx >> put by


----------------------------------------------------------------
-- Histogram
----------------------------------------------------------------

instance (Serialize a, G.Vector v a, Bin bin, Serialize bin
         ) => Serialize (Histogram v bin a) where
  get = do b  <- get
           uo <- get
           v  <- G.replicateM (nBins b) get
           return $! histogramUO b uo v
  put h = do put (bins h)
             put (outOfRange h)
             G.mapM_ put (histData h)
