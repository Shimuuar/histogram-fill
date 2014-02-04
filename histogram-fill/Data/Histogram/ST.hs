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
                         , fill
                         -- , fillMonoidAccum
                         , unsafeFreezeHist
                         , freezeHist
                         ) where

import Control.Monad
import Control.Monad.Primitive

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import Data.Histogram.Generic

----------------------------------------------------------------
-- Mutable histograms
----------------------------------------------------------------

-- | Mutable histogram.
data MHistogram s v bin a =
  MHistogram
    {-# UNPACK #-} !Int -- Number of bins
    !bin                -- Binning
    !(v s a)            -- Bin contents. Underflows are stored at the
                        -- n'th index and overflow are in the n+1


-- | Create new mutable histogram. All bins are set to zero element as
--   passed to function.
newMHistogram :: (PrimMonad m, Bin bin, M.MVector v a) => a -> bin -> m (MHistogram (PrimState m) v bin a)
newMHistogram zero bin = do
  let n = nBins bin
  -- NOTE: replicate will create vector of zero length requested
  --       length is negative. Thus if number of bins is negative buffer
  --       will be shorter than 2. And it's assumed that there's always at
  --       least 2 bin. Consequently it could lead to memory corruption.
  when (n < 0) $
    error "Data.Histogram.ST.newMHistogram: negative number of bins"
  a  <- M.replicate (n + 2) zero
  return $ MHistogram n bin a
{-# INLINE newMHistogram #-}

-- | Generic fill. It could be seen as left fold with multiple
--   accumulators where accumulator is chosen by @BinValue bin@.
fill :: (PrimMonad m, M.MVector v a, Bin bin)
     => MHistogram (PrimState m) v bin a -- ^ Mutable histogram to put value to
     -> BinValue bin                     -- ^ Value being binned
     -> (a -> b -> a)                    -- ^ Fold function
     -> b                                -- ^ Value being put into histogram
     -> m ()
fill (MHistogram n bin arr) !x f val = do
  a <- M.unsafeRead arr ix
  M.unsafeWrite arr ix $! f a val
  where
    i  = toIndex bin x
    ix | i <  0    = n
       | i >= n    = n+1
       | otherwise = i
{-# INLINE fill #-}


-- | Create immutable histogram from mutable one. This operation is
-- unsafe! Accumulator mustn't be used after that
unsafeFreezeHist :: (PrimMonad m, G.Vector v a, Bin bin) 
                 => MHistogram (PrimState m) (G.Mutable v) bin a 
                 -> m (Histogram v bin a)
unsafeFreezeHist (MHistogram n bin arr) = do
  u <- M.unsafeRead arr  n
  o <- M.unsafeRead arr (n+1)
  a <- G.unsafeFreeze $ M.slice 0 n arr
  return $ histogramUO bin (Just (u,o)) a
{-# INLINE unsafeFreezeHist #-}  

-- | Create immutable histogram from mutable one.
freezeHist :: (PrimMonad m, G.Vector v a, Bin bin) 
           => MHistogram (PrimState m) (G.Mutable v) bin a 
           -> m (Histogram v bin a)
freezeHist (MHistogram n bin arr) = do
  u <- M.unsafeRead arr  n
  o <- M.unsafeRead arr (n+1)
  a <- G.freeze $ M.slice 0 n arr
  return $ histogramUO bin (Just (u,o)) a
{-# INLINE freezeHist #-}

