{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module     : Data.Histogram.ST
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Mutable histograms.

module Data.Histogram.ST ( -- * Mutable histograms
                           MHistogram(..)
                         , newMHistogram
                         , fillOne
                         , fillOneW
                         , fillMonoid
                         , freezeHist
                         ) where


import Control.Monad.ST

import Data.Monoid
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Generic as G

import Data.Histogram

----------------------------------------------------------------
-- Mutable histograms
----------------------------------------------------------------

-- | Mutable histogram.
data MHistogram s bin a where
    MHistogram :: (Bin bin, MU.Unbox a) => 
                  bin            -- ^ Binning
               -> MU.MVector s a -- ^ Over/underflows
               -> MU.MVector s a -- ^ Data
               -> MHistogram s bin a

-- | Create new mutable histogram. All bins are set to zero element as
--   passed to function.
newMHistogram :: (Bin bin, U.Unbox a) => a -> bin -> ST s (MHistogram s bin a)
newMHistogram zero bin = do
  uo <- MU.newWith 2 zero
  a  <- MU.newWith (nBins bin) zero
  return $ MHistogram bin uo a

-- | Put one value into histogram
fillOne :: Num a => MHistogram s bin a -> BinValue bin -> ST s ()
fillOne (MHistogram bin uo arr) x
    | i < 0              = MU.write uo  0 . (+1)  =<< MU.read uo 0
    | i >= MU.length arr = MU.write uo  1 . (+1)  =<< MU.read uo 1
    | otherwise          = MU.write arr i . (+1)  =<< MU.read arr i
    where
      i = toIndex bin x

-- | Put one value into histogram with weight
fillOneW :: Num a => MHistogram s bin a -> (BinValue bin, a) -> ST s ()
fillOneW (MHistogram bin uo arr) (x,w)
    | i < 0              = MU.write uo  0 . (+w)  =<< MU.read uo 0
    | i >= MU.length arr = MU.write uo  1 . (+w)  =<< MU.read uo 1
    | otherwise          = MU.write arr i . (+w)  =<< MU.read arr i
    where
      i = toIndex bin x

-- | Put one monoidal element
fillMonoid :: Monoid a => MHistogram s bin a -> (BinValue bin, a) -> ST s ()
fillMonoid (MHistogram bin uo arr) (x,m)
    | i < 0              = MU.write uo  1 . (flip mappend m)  =<< MU.read uo  0
    | i >= MU.length arr = MU.write uo  1 . (flip mappend m)  =<< MU.read uo  1
    | otherwise          = MU.write arr i . (flip mappend m)  =<< MU.read arr i
    where 
      i = toIndex bin x

-- | Create immutable histogram from mutable one. This operation involve copying.
freezeHist :: MHistogram s bin a -> ST s (Histogram bin a)
freezeHist (MHistogram bin uo arr) = do
  u <- MU.read uo 0
  o <- MU.read uo 1
  -- Copy array
  let len = MU.length arr
  tmp  <- MU.new len
  MU.copy tmp arr
  a    <- G.unsafeFreeze tmp
  return $ Histogram bin (Just (u,o)) a


