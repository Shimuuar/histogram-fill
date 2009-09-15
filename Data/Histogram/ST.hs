{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
module Data.Histogram.ST ( -- * Mutable histograms
                           HistogramST(..)
                         , newHistogramST
                         , fillOne
                         , fillOneW
                         , fillMonoid
                         , freezeHist

                         -- * Accumulators
                         , Accumulator(..)
                         , Accum(Accum)

                         , accumList
                         , accumHist

                         , fillHistograms
                         ) where


import Control.Monad.ST

import Data.STRef
import Data.Array.Vector
import Data.Monoid

import Data.Histogram
import Data.Histogram.Bin


----------------------------------------------------------------
-- Mutable histograms
----------------------------------------------------------------

-- | Mutable histogram
data HistogramST s bin a where
    HistogramST :: (Bin bin, UA a) => 
                   bin
                -> STRef s a
                -> STRef s a
                -> MUArr a s
                -> HistogramST s bin a

-- | Create new mutable histogram. All bins are set to zero.
newHistogramST :: (Bin bin, UA a, Num a) => bin -> ST s (HistogramST s bin a)
newHistogramST bin = do u <- newSTRef 0
                        o <- newSTRef 0
                        a <- newMU (nBins bin)
                        mapM_ (\i -> writeMU a i 0) [0 .. (lengthMU a) - 1]
                        return $ HistogramST bin u o a

-- | Put one value into histogram
fillOne :: Num a => HistogramST s bin a -> BinValue bin -> ST s ()
fillOne (HistogramST bin u o arr) x
    | i < 0             = modifySTRef u ((+1) $!)
    | i >= lengthMU arr = modifySTRef o ((+1) $!)
    | otherwise         = writeMU arr i . (+1)  =<< readMU arr i
    where
      i = toIndex bin x

-- | Put one value into histogram with weight
fillOneW :: Num a => HistogramST s bin a -> (BinValue bin, a) -> ST s ()
fillOneW (HistogramST bin u o arr) (x,w)
    | i < 0             = modifySTRef u ((+w) $!)
    | i >= lengthMU arr = modifySTRef o ((+w) $!)
    | otherwise         = writeMU arr i . (+w)  =<< readMU arr i
    where
      i = toIndex bin x

-- | Put one monoidal element
fillMonoid :: Monoid a => HistogramST s bin a -> (BinValue bin, a) -> ST s ()
fillMonoid (HistogramST bin u o arr) (x,m)
    | i < 0             = modifySTRef u ((flip mappend m) $!)
    | i >= lengthMU arr = modifySTRef o ((flip mappend m) $!)
    | otherwise         = writeMU arr i . (flip mappend m)  =<< readMU arr i
    where
      i = toIndex bin x

-- | Create immutable histogram from mutable one. This operation involve copying.
freezeHist :: HistogramST s bin a -> ST s (Histogram bin a)
freezeHist (HistogramST bin und over arr) = do
  u <- readSTRef und
  o <- readSTRef over
  -- Copy array
  let len = lengthMU arr
  tmp  <- newMU len
  memcpyOffMU arr tmp 0 0 len
  a    <- unsafeFreezeAllMU tmp
  return $ Histogram bin (u,o) a



----------------------------------------------------------------
-- Accumulator typeclass
----------------------------------------------------------------
-- | This is class with accumulation semantics. It's used to fill many
-- histogram at once. It accept values of type a and return data of type b.
class Accumulator h where
    -- | Put one element into accumulator
    putOne  :: h s a b -> a   -> ST s () 
    -- | Extract data from historam
    extract :: Monoid b => (h s a b) -> ST s b

-- | Put many elements in histogram(s) at once 
putMany :: Accumulator h => h s a b -> [a] -> ST s () 
putMany !h = mapM_ (putOne h) 

-- | Put all values into histogram and return result
fillHistograms :: Monoid b => (forall s . ST s (Accum s a b)) -> [a] -> b
fillHistograms h xs = runST $ do h' <- h
                                 putMany h' xs
                                 extract h'

----------------------------------------------------------------
-- GADT wrapper 
----------------------------------------------------------------
-- | Abstract wrapper for histograms. 
data Accum s a b where
    Accum :: Accumulator h => h s a b -> Accum s a b

instance Accumulator Accum where
    putOne  !(Accum h) !x = putOne h x 
    extract !(Accum h)    = extract h


----------------------------------------------------------------
-- List of histograms
----------------------------------------------------------------
newtype AccumList s a b = AccumList [Accum s a b]
 
-- | Wrap list of histograms into one 'Accum'
accumList :: [ST s (Accum s a b)] -> ST s (Accum s a b)
accumList l = (Accum . AccumList) `fmap` sequence l

instance Accumulator AccumList where
    putOne  !(AccumList l) !x = mapM_ (flip putOne $ x) l 
    extract !(AccumList l)    = mconcat `fmap` mapM extract l 


----------------------------------------------------------------
-- Generic histogram 
----------------------------------------------------------------
data AccumHist s a b where
    AccumHist :: (Bin bin) =>
                 (a -> HistogramST s bin val -> ST s ())
              -> (Histogram bin val -> b)
              -> HistogramST s bin val
              -> AccumHist s a b

-- | Accumulator for arbitrary 'HistogramST' based histogram
accumHist :: (Bin bin) =>
             (a -> HistogramST s bin val -> ST s ())
          -> (Histogram bin val -> b)
          -> HistogramST s bin val
          -> ST s (Accum s a b)
accumHist inp out h = return . Accum $ AccumHist inp out h

instance Accumulator AccumHist where
    putOne  !(AccumHist inp _ st) !x = inp x st
    extract !(AccumHist _ out st)    = out `fmap` freezeHist st
