{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-- | Cereal instances for histogram-fill
module Data.Histogram.Binary (
  ) where

import Control.Applicative
import Data.Binary
import qualified Data.Vector.Generic as G

import Data.Histogram.Bin
import Data.Histogram.Bin.MaybeBin
import Data.Histogram.Bin.BinVar
import Data.Histogram.Generic (Histogram, histogramUO, histData, outOfRange, bins)



----------------------------------------------------------------
-- Bins
----------------------------------------------------------------

instance Binary BinI where
  get   = binI <$> get <*> get
  put b = do put (lowerLimit b)
             put (upperLimit b)

instance Binary BinInt where
  get   = binIntStep <$> get <*> get <*> get
  put b = do put (lowerLimit b)
             put (binSize    b)
             put (nBins      b)

instance (RealFrac f, Binary f) => Binary (BinF f) where
  get   = binFstep <$> get <*> get <*> get
  put b = do put (lowerLimit b)
             put (binSize    b)
             put (nBins      b)

instance Binary BinD where
  get   = binDstep <$> get <*> get <*> get
  put b = do put (lowerLimit b)
             put (binSize    b)
             put (nBins      b)

instance Binary LogBinD where
   get   = logBinDN <$> get <*> get <*> get
   put b = do put (lowerLimit       b)
              put (logBinDIncrement b)
              put (nBins            b)

instance Binary (BinEnum a) where
  get = BinEnum <$> get
  put (BinEnum b) = put b

instance (Binary bX, Binary bY) => Binary (Bin2D bX bY) where
  get = Bin2D <$> get <*> get
  put (Bin2D bx by) = put bx >> put by

deriving instance (Binary bin) => Binary (MaybeBin bin)

instance (G.Vector v a, G.Vector v Bool, Binary a, Ord a) => Binary (BinVarG v a) where
  get = binVar <$> do n <- get
                      G.replicateM n get
  put b = do let v = cuts b
             put (G.length v)
             G.forM_ v put


----------------------------------------------------------------
-- Histogram
----------------------------------------------------------------

instance (Binary a, G.Vector v a, Bin bin, Binary bin
         ) => Binary (Histogram v bin a) where
  get = do b  <- get
           uo <- get
           v  <- G.replicateM (nBins b) get
           return $! histogramUO b uo v
  put h = do put (bins h)
             put (outOfRange h)
             G.mapM_ put (histData h)
