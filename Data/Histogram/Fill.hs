{-# LANGUAGE GADTs        #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module     : Data.Histogram.Fill
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Module with algorithms for histogram filling. This is pure wrapper
-- around stateful histograms.
--
module Data.Histogram.Fill ( -- * Type classes
                             HistBuilder(..)
                           -- * Stateful histogram builders
                           , HBuilderST
                           , feedOne
                           , freezeHBuilderST
                           , HBuilder(unwrapST)
                           , joinHBuilder
                           , joinHBuilderList
                           , treeHBuilder
                           , joinHBuilderST
                           , joinHBuilderSTList
                           , treeHBuilderST
                           -- * Fill histograms
                           , fillBuilderST
                           -- * Histogram constructors
                           , module Data.Histogram.Bin
                           -- ** Fixed weigth histograms
                           , mkHist1
                           , mkHist
                           , mkHistMaybe
                           -- ** Weighted histograms
                           , mkHistWgh1
                           , mkHistWgh
                           , mkHistWghMaybe
                           -- ** Histograms with monoidal bins
                           , mkHistMonoid1
                           , mkHistMonoid
                           , mkHistMonoidMaybe
                           -- * Auxillary functions
                           , forceInt
                           , forceDouble
                           , forceFloat
                           ) where

import Control.Applicative ((<$>))
import Control.Monad    (when)
import Control.Monad.ST (ST,runST)
import Data.Monoid      (Monoid, mempty)

import qualified Data.Vector.Unboxed.Mutable as MU

import Data.Histogram
import Data.Histogram.Bin
import Data.Histogram.ST

----------------------------------------------------------------

-- | Histogram builder typeclass. Instance of this class contain
--   instructions how to build histograms.
class HistBuilder h where
    -- | Convert input type of histogram from a to a'
    modifyIn  :: (a' -> a) -> h a b -> h a' b
    -- | Add cut to histogram. Only put value histogram if condition is true.
    addCut    :: (a -> Bool) -> h a b -> h a b
    -- | 
    modifyMaybe :: h a b -> h (Maybe a) b
    -- | Convert output of histogram
    modifyOut :: (b -> b') -> h a b -> h a  b'

----------------------------------------------------------------

-- | Stateful histogram builder.
data HBuilderST s a b = HBuilderST { hbInput  :: a -> ST s ()
                                   , hbOutput :: ST s b
                                   }

instance HistBuilder (HBuilderST s) where
    modifyIn  f h = h { hbInput  = hbInput h . f }
    addCut    f h = h { hbInput  = \x -> when (f x) (hbInput h x) }
    modifyMaybe h = h { hbInput  = modified } 
        where modified (Just x) = hbInput h x
              modified Nothing  = return ()
    modifyOut f h = h { hbOutput = f `fmap` hbOutput h }

instance Functor (HBuilderST s a) where
    fmap = modifyOut

-- | Put one value into histogram
feedOne :: HBuilderST s a b -> a -> ST s ()
feedOne = hbInput

-- | Create stateful histogram from instructions. Histograms could
--   be filled either in the ST monad or with createHistograms
freezeHBuilderST :: HBuilderST s a b -> ST s b
freezeHBuilderST = hbOutput

newtype HBuilder a b = HBuilder { unwrapST :: (forall s . ST s (HBuilderST s a b)) }

instance HistBuilder (HBuilder) where
    modifyIn  f (HBuilder h) = HBuilder (modifyIn  f <$> h)
    addCut    f (HBuilder h) = HBuilder (addCut    f <$> h)
    modifyMaybe (HBuilder h) = HBuilder (modifyMaybe <$> h)
    modifyOut f (HBuilder h) = HBuilder (modifyOut f <$> h)

instance Functor (HBuilder a) where
    fmap = modifyOut

-- | Join list of builders into one builder
joinHBuilderST :: [HBuilderST s a b] -> HBuilderST s a [b]
joinHBuilderST hs = HBuilderST { hbInput  = \x -> mapM_ (flip hbInput x) hs
                               , hbOutput = mapM hbOutput hs
                               }

-- | Join list of builders into one builders
joinHBuilderSTList :: [HBuilderST s a [b]] -> HBuilderST s a [b]
joinHBuilderSTList = modifyOut concat . joinHBuilderST

-- | Join list of builders
joinHBuilder :: [HBuilder a b] -> HBuilder a [b]
joinHBuilder hs = HBuilder (joinHBuilderST <$> mapM unwrapST hs)

-- | Join list of builders
joinHBuilderList :: [HBuilder a [b]] -> HBuilder a [b]
joinHBuilderList = modifyOut concat . joinHBuilder

treeHBuilderST :: [HBuilderST s a b -> HBuilderST s a' b'] -> HBuilderST s a b -> HBuilderST s a' [b']
treeHBuilderST fs h = joinHBuilderST $ map ($ h) fs

treeHBuilder :: [HBuilder a b -> HBuilder a' b'] -> HBuilder a b -> HBuilder a' [b']
treeHBuilder fs h = joinHBuilder $ map ($ h) fs

fillBuilderST :: (HBuilder a b) -> [a] -> b
fillBuilderST (HBuilder hb) xs = 
    runST $ do h <- hb
               mapM_ (feedOne h) xs
               freezeHBuilderST h


----------------------------------------------------------------
-- Histogram constructors
----------------------------------------------------------------

-- | Create histogram builder which take single item as input. Each
--   item has weight 1.
mkHist1 :: (Bin bin, MU.Unbox val, Num val) =>
           bin                      -- ^ Bin information
        -> (Histogram bin val -> b) -- ^ Output function 
        -> (a -> BinValue bin)      -- ^ Input function
        -> HBuilder a b
mkHist1 bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderST { hbInput  = fillOne acc . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }

-- | Create histogram builder which take many items as input. Each
--   item has weight 1.
mkHist :: (Bin bin, MU.Unbox val, Num val) =>
          bin                      -- ^ Bin information
       -> (Histogram bin val -> b) -- ^ Output function
       -> (a -> [BinValue bin])    -- ^ Input function 
       -> HBuilder a b
mkHist bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderST { hbInput  = mapM_ (fillOne acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }

-- | Create histogram builder which at most one item as input. Each
--   item has weight 1. 
mkHistMaybe :: (Bin bin, MU.Unbox val, Num val) =>
          bin                         -- ^ Bin information
       -> (Histogram bin val -> b)    -- ^ Output function
       -> (a -> Maybe (BinValue bin)) -- ^ Input function 
       -> HBuilder a b
mkHistMaybe bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderST { hbInput  = maybe (return ()) (fillOne acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }

-- | Create histogram with weighted bin. Takes one item at time. 
mkHistWgh1 :: (Bin bin, MU.Unbox val, Num val) =>
              bin                        -- ^ Bin information
          -> (Histogram bin val -> b)    -- ^ Output function
          -> (a -> (BinValue bin, val))  -- ^ Input function
          -> HBuilder a b
mkHistWgh1 bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderST { hbInput  = fillOneW acc . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }

-- | Create histogram with weighted bin. Takes many items at time.
mkHistWgh :: (Bin bin, MU.Unbox val, Num val) => 
             bin                          -- ^ Bin information
          -> (Histogram bin val  -> b)    -- ^ Output function
          -> (a -> [(BinValue bin, val)]) -- ^ Input function
          -> HBuilder a b
mkHistWgh bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderST { hbInput  = mapM_ (fillOneW acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }

-- | Create histogram with weighted bin. Takes many items at time.
mkHistWghMaybe :: (Bin bin, MU.Unbox val, Num val) => 
                  bin                              -- ^ Bin information
               -> (Histogram bin val  -> b)        -- ^ Output function
               -> (a -> Maybe (BinValue bin, val)) -- ^ Input function
               -> HBuilder a b
mkHistWghMaybe bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderST { hbInput  = maybe (return ()) (fillOneW acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }

-- | Create histogram with monoidal bins
mkHistMonoid1 :: (Bin bin, MU.Unbox val, Monoid val) =>
              bin                         -- ^ Bin information
          -> (Histogram bin val -> b)     -- ^ Output function
          -> (a -> (BinValue bin, val))   -- ^ Input function
          -> HBuilder a b
mkHistMonoid1 bin out inp = HBuilder $ do
  acc <- newMHistogram mempty bin
  return $ HBuilderST { hbInput  = fillMonoid acc . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }

-- | Create histogram with monoidal bins. Takes many items at time.
mkHistMonoid :: (Bin bin, MU.Unbox val, Monoid val) =>
              bin                         -- ^ Bin information
          -> (Histogram bin val -> b)     -- ^ Output function
          -> (a -> [(BinValue bin, val)]) -- ^ Input function
          -> HBuilder a b
mkHistMonoid bin out inp = HBuilder $ do
  acc <- newMHistogram mempty bin
  return $ HBuilderST { hbInput  = mapM_ (fillMonoid acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }

-- | Create histogram with monoidal bins
mkHistMonoidMaybe :: (Bin bin, MU.Unbox val, Monoid val) =>
                     bin                              -- ^ Bin information
                  -> (Histogram bin val -> b)         -- ^ Output function
                  -> (a -> Maybe (BinValue bin, val)) -- ^ Input function
                  -> HBuilder a b
mkHistMonoidMaybe bin out inp = HBuilder $ do
  acc <- newMHistogram mempty bin
  return $ HBuilderST { hbInput  = maybe (return ()) (fillMonoid acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }

----------------------------------------------------------------

-- | Function used to restrict type of histrogram.
forceInt :: Histogram bin Int -> Histogram bin Int
forceInt = id

-- | Function used to restrict type of histrogram.
forceDouble :: Histogram bin Double -> Histogram bin Double
forceDouble = id

-- | Function used to restrict type of histrogram.
forceFloat :: Histogram bin Float -> Histogram bin Float
forceFloat = id
