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
  put b = do put (lowerLimit b)
             put (upperLimit b)

instance Serialize BinInt where
  get   = binIntStep <$> get <*> get <*> get
  put b = do put (lowerLimit b)
             put (binSize    b)
             put (nBins      b)

instance (RealFrac f, Serialize f) => Serialize (BinF f) where
  get   = binFstep <$> get <*> get <*> get
  put b = do put (lowerLimit b)
             put (binSize    b)
             put (nBins      b)

instance Serialize BinD where
  get   = binDstep <$> get <*> get <*> get
  put b = do put (lowerLimit b)
             put (binSize    b)
             put (nBins      b)

instance Serialize LogBinD where
   get   = logBinDN <$> get <*> get <*> get
   put b = do put (lowerLimit       b)
              put (logBinDIncrement b)
              put (nBins            b)

instance Serialize (BinEnum a) where
  get = BinEnum <$> get
  put (BinEnum b) = put b

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
