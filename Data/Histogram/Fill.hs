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
                           , HistBuilderST(..)
                           -- * Stateful histogram builders
                           , HBuilder
                           , HBuilderST(unwrapST)
                           , hbuilderTree
                           , hbuilderTreeST
                           , joinHBuilder
                           , joinHBuilderList
                           , joinHBuilderST
                           , joinHBuilderListST
                           -- * Fill histograms
                           , fillBuilderST
                           -- * Histogram constructors
                           , module Data.Histogram.Bin
                           , mkHist
                           , mkHist1
                           , mkHistWgh
                           , mkHistWgh1
                           , mkHistMonoid
                           , mkHistMonoid1
                           -- ** Force output type
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
    -- | Convert output of histogram
    modifyOut :: (b -> b') -> h a b -> h a  b'

class HistBuilderST h where
    -- | Put one value into histogram
    feedOne :: h s a b -> a -> ST s ()
    -- | Put one value into histogram
    feedMany :: h s a b -> [a] -> ST s ()
    feedMany h = mapM_ (feedOne h)
    -- | Create stateful histogram from instructions. Histograms could
    --   be filled either in the ST monad or with createHistograms
    freezeHB :: h s a b -> ST s b

----------------------------------------------------------------

-- | Single stateful histogram
data HBuilder s a b = HBuilder { hbInput  :: a -> ST s ()
                               , hbOutput :: ST s b
                               }

instance HistBuilder (HBuilder s) where
    modifyIn  f h = h { hbInput  = hbInput h . f }
    addCut    f h = h { hbInput  = \x -> when (f x) (hbInput h x) }
    modifyOut f h = h { hbOutput = f `fmap` hbOutput h }

instance HistBuilderST HBuilder where
    feedOne  = hbInput
    freezeHB = hbOutput

newtype HBuilderST a b = HBuilderST { unwrapST :: (forall s . ST s (HBuilder s a b)) }

instance HistBuilder (HBuilderST) where
    modifyIn  f (HBuilderST h) = HBuilderST (modifyIn  f <$> h)
    addCut    f (HBuilderST h) = HBuilderST (addCut    f <$> h)
    modifyOut f (HBuilderST h) = HBuilderST (modifyOut f <$> h)


-- | Join list of builders into one builder
joinHBuilder :: [HBuilder s a b] -> HBuilder s a [b]
joinHBuilder hs = HBuilder { hbInput  = \x -> mapM_ (flip hbInput x) hs
                           , hbOutput = mapM hbOutput hs
                           }

-- | Join list of builders into one builders
joinHBuilderList :: [HBuilder s a [b]] -> HBuilder s a [b]
joinHBuilderList = modifyOut concat . joinHBuilder

-- | Join list of builders
joinHBuilderST :: [HBuilderST a b] -> HBuilderST a [b]
joinHBuilderST hs = HBuilderST (joinHBuilder <$> mapM unwrapST hs)

-- | Join list of builders
joinHBuilderListST :: [HBuilderST a [b]] -> HBuilderST a [b]
joinHBuilderListST = modifyOut concat . joinHBuilderST


hbuilderTree :: [HBuilder s a b -> HBuilder s a' b'] -> HBuilder s a b -> HBuilder s a' [b']
hbuilderTree fs h = joinHBuilder $ map ($ h) fs

hbuilderTreeST :: [HBuilderST a b -> HBuilderST a' b'] -> HBuilderST a b -> HBuilderST a' [b']
hbuilderTreeST fs h = joinHBuilderST $ map ($ h) fs

fillBuilderST :: (HBuilderST a b) -> [a] -> b
fillBuilderST (HBuilderST hb) xs = 
    runST $ do h <- hb
               feedMany h xs
               freezeHB h


----------------------------------------------------------------
-- Histogram constructors
----------------------------------------------------------------

-- | Create histogram builder which take single item as input. Each
--   item has weight 1. To set type of bin 'force*' function could be used.
mkHist1 :: (Bin bin, MU.Unbox val, Num val) =>
           bin                      -- ^ Bin information
        -> (Histogram bin val -> b) -- ^ Output function 
        -> (a -> BinValue bin)      -- ^ Input function
        -> HBuilderST a b
mkHist1 bin out inp = HBuilderST $ do
  acc <- newMHistogram 0 bin
  return $ HBuilder { hbInput  = fillOne acc . inp
                    , hbOutput = fmap out (freezeHist acc)
                    }

-- | Create histogram builder which take many items a3s input. Each
--   item has weight 1. To set type of bin 'force*' function could be
--   used.
mkHist :: (Bin bin, MU.Unbox val, Num val) =>
          bin                      -- ^ Bin information
       -> (Histogram bin val -> b) -- ^ Output function
       -> (a -> [BinValue bin])    -- ^ Input function 
       -> HBuilderST a b
mkHist bin out inp = HBuilderST $ do
  acc <- newMHistogram 0 bin
  return $ HBuilder { hbInput  = mapM_ (fillOne acc) . inp
                    , hbOutput = fmap out (freezeHist acc)
                    }

-- | Create histogram with weighted bin. Takes one item at time. 
mkHistWgh1 :: (Bin bin, MU.Unbox val, Num val) =>
              bin                        -- ^ Bin information
          -> (Histogram bin val -> b)    -- ^ Output function
          -> (a -> (BinValue bin, val))  -- ^ Input function
          -> HBuilderST a b
mkHistWgh1 bin out inp = HBuilderST $ do
  acc <- newMHistogram 0 bin
  return $ HBuilder { hbInput  = fillOneW acc . inp
                    , hbOutput = fmap out (freezeHist acc)
                    }


-- | Create histogram with weighted bin. Takes many items at time.
mkHistWgh :: (Bin bin, MU.Unbox val, Num val) => 
             bin                          -- ^ Bin information
          -> (Histogram bin val  -> b)    -- ^ Output function
          -> (a -> [(BinValue bin, val)]) -- ^ Input function
          -> HBuilderST a b
mkHistWgh bin out inp = HBuilderST $ do
  acc <- newMHistogram 0 bin
  return $ HBuilder { hbInput  = mapM_ (fillOneW acc) . inp
                    , hbOutput = fmap out (freezeHist acc)
                    }

-- | Create histogram with monoidal bins
mkHistMonoid1 :: (Bin bin, MU.Unbox val, Monoid val) =>
              bin                         -- ^ Bin information
          -> (Histogram bin val -> b)     -- ^ Output function
          -> (a -> (BinValue bin, val))   -- ^ Input function
          -> HBuilderST a b
mkHistMonoid1 bin out inp = HBuilderST $ do
  acc <- newMHistogram mempty bin
  return $ HBuilder { hbInput  = fillMonoid acc . inp
                    , hbOutput = fmap out (freezeHist acc)
                    }

-- | Create histogram with monoidal bins. Takes many items at time.
mkHistMonoid :: (Bin bin, MU.Unbox val, Monoid val) =>
              bin                         -- ^ Bin information
          -> (Histogram bin val -> b)     -- ^ Output function
          -> (a -> [(BinValue bin, val)]) -- ^ Input function
          -> HBuilderST a b
mkHistMonoid bin out inp = HBuilderST $ do
  acc <- newMHistogram mempty bin
  return $ HBuilder { hbInput  = mapM_ (fillMonoid acc) . inp
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
