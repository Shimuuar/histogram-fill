{-# LANGUAGE BangPatterns #-}
-- |
-- Module     : Data.Histogram.ST
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
-- Mutable histograms.

module Data.Histogram.ST ( -- * Mutable histograms
                           MHistogram
                         , newMHistogram
                         , fillOne
                         , fillOneW
                         , fillMonoid
                         -- , fillMonoidAccum
                         , unsafeFreezeHist
                         , freezeHist
                         ) where

import Control.Monad.Primitive

import Data.Monoid
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import Data.Histogram.Generic

----------------------------------------------------------------
-- Mutable histograms
----------------------------------------------------------------

-- | Mutable histogram.
data MHistogram s v bin a = MHistogram 
                            !bin     -- Bin
                            !(v s a) -- Under/overflows
                            !(v s a) -- Bin contents


-- | Create new mutable histogram. All bins are set to zero element as
--   passed to function.
newMHistogram :: (PrimMonad m, Bin bin, M.MVector v a) => a -> bin -> m (MHistogram (PrimState m) v bin a)
newMHistogram zero bin = do
  uo <- M.replicate 2 zero
  a  <- M.replicate (nBins bin) zero
  return $ MHistogram bin uo a
{-# INLINE newMHistogram #-}

-- | Put one value into histogram
fillOne :: (PrimMonad m, Num a, M.MVector v a, Bin bin) => MHistogram (PrimState m) v bin a -> BinValue bin -> m ()
fillOne (MHistogram bin uo arr) !x
    | i < 0             = M.unsafeWrite uo  0 . (+1) =<< M.unsafeRead uo 0
    | i >= M.length arr = M.unsafeWrite uo  1 . (+1) =<< M.unsafeRead uo 1
    | otherwise         = M.unsafeWrite arr i . (+1) =<< M.unsafeRead arr i
    where
      i = toIndex bin x
{-# INLINE fillOne #-}

-- | Put one value into histogram with weight
fillOneW :: (PrimMonad m, Num a, M.MVector v a, Bin bin) => MHistogram (PrimState m) v bin a -> (BinValue bin, a) -> m ()
fillOneW (MHistogram bin uo arr) (!x,!w)
    | i < 0             = M.unsafeWrite uo  0 . (+w) =<< M.unsafeRead uo 0
    | i >= M.length arr = M.unsafeWrite uo  1 . (+w) =<< M.unsafeRead uo 1
    | otherwise         = M.unsafeWrite arr i . (+w) =<< M.unsafeRead arr i
    where
      i = toIndex bin x
{-# INLINE fillOneW #-} 

-- | Put one monoidal element
fillMonoid :: (PrimMonad m, Monoid a, M.MVector v a, Bin bin) => MHistogram (PrimState m) v bin a -> (BinValue bin, a) -> m ()
fillMonoid (MHistogram bin uo arr) (!x,!m)
    | i < 0             = M.unsafeWrite uo  0 . flip mappend m =<< M.unsafeRead uo  0
    | i >= M.length arr = M.unsafeWrite uo  1 . flip mappend m =<< M.unsafeRead uo  1
    | otherwise         = M.unsafeWrite arr i . flip mappend m =<< M.unsafeRead arr i
    where 
      i = toIndex bin x
{-# INLINE fillMonoid #-}


-- | Create immutable histogram from mutable one. This operation is
-- unsafe! Accumulator mustn't be used after that
unsafeFreezeHist :: (PrimMonad m, G.Vector v a, Bin bin) 
                 => MHistogram (PrimState m) (G.Mutable v) bin a 
                 -> m (Histogram v bin a)
unsafeFreezeHist (MHistogram bin uo arr) = do
  u <- M.unsafeRead uo 0
  o <- M.unsafeRead uo 1
  a <- G.unsafeFreeze arr
  return $ histogramUO bin (Just (u,o)) a
{-# INLINE unsafeFreezeHist #-}  

-- | Create immutable histogram from mutable one.
freezeHist :: (PrimMonad m, G.Vector v a, Bin bin) 
           => MHistogram (PrimState m) (G.Mutable v) bin a 
           -> m (Histogram v bin a)
freezeHist (MHistogram bin uo arr) = do
  u <- M.unsafeRead uo 0
  o <- M.unsafeRead uo 1
  a <- G.freeze
  return $ histogramUO bin (Just (u,o)) a
{-# INLINE freezeHist #-}

