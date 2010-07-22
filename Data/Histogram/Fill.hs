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
                             -- * Histogram builders
                             -- ** Stateful
                           , HBuilderST
                           , feedOne
                           , freezeHBuilderST
                           , joinHBuilderST
                           , joinHBuilderSTList
                           , treeHBuilderST
                             -- ** IO based
                           , HBuilderIO
                           , feedOneIO
                           , freezeHBuilderIO
                           , joinHBuilderIO
                           , joinHBuilderIOList
                           , treeHBuilderIO
                             -- ** Stateless
                           , HBuilder
                           , joinHBuilder
                           , joinHBuilderList
                           , treeHBuilder
                             -- ** Conversion between builders
                           , toBuilderST
                           , toBuilderIO
                           , builderSTtoIO
                           -- * Fill histograms
                           , fillBuilder
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
import Control.Monad       (when)
import Control.Monad.ST 

import Data.Monoid         (Monoid, mempty)
import Data.Vector.Unboxed (Unbox)

import Data.Histogram
import Data.Histogram.Bin
import Data.Histogram.ST

----------------------------------------------------------------
-- Type class
----------------------------------------------------------------

-- | Histogram builder typeclass. Instance of this class contain
--   instructions how to build histograms.
class HistBuilder h where
    -- | Convert input type of histogram from a to a'
    modifyIn  :: (a' -> a) -> h a b -> h a' b
    -- | Make input function accept value only if it's Just a.
    modifyMaybe :: h a b -> h (Maybe a) b
    -- | Add cut to histogram. Only put value histogram if condition is true.
    addCut    :: (a -> Bool) -> h a b -> h a b
    -- | Convert output of histogram
    modifyOut :: (b -> b') -> h a b -> h a  b'

----------------------------------------------------------------
-- ST based builder
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


-- | Join list of builders into one builder
joinHBuilderST :: [HBuilderST s a b] -> HBuilderST s a [b]
joinHBuilderST hs = HBuilderST { hbInput  = \x -> mapM_ (flip hbInput x) hs
                               , hbOutput = mapM hbOutput hs
                               }

-- | Join list of builders into one builders
joinHBuilderSTList :: [HBuilderST s a [b]] -> HBuilderST s a [b]
joinHBuilderSTList = fmap concat . joinHBuilderST

treeHBuilderST :: [HBuilderST s a b -> HBuilderST s a' b'] -> HBuilderST s a b -> HBuilderST s a' [b']
treeHBuilderST fs h = joinHBuilderST $ map ($ h) fs

----------------------------------------------------------------
-- IO based
----------------------------------------------------------------

-- | Stateful histogram builder.
data HBuilderIO a b = HBuilderIO { hbInputIO  :: a -> IO ()
                                 , hbOutputIO :: IO b
                                 }

instance HistBuilder (HBuilderIO) where
    modifyIn  f h = h { hbInputIO  = hbInputIO h . f }
    addCut    f h = h { hbInputIO  = \x -> when (f x) (hbInputIO h x) }
    modifyMaybe h = h { hbInputIO  = modified } 
        where modified (Just x) = hbInputIO h x
              modified Nothing  = return ()
    modifyOut f h = h { hbOutputIO = f `fmap` hbOutputIO h }

instance Functor (HBuilderIO a) where
    fmap = modifyOut

-- | Put one value into histogram
feedOneIO :: HBuilderIO a b -> a -> IO ()
feedOneIO = hbInputIO

-- | Create stateful histogram from instructions. Histograms could
--   be filled either in the ST monad or with createHistograms
freezeHBuilderIO :: HBuilderIO a b -> IO b
freezeHBuilderIO = hbOutputIO

-- | Join list of builders into one builder
joinHBuilderIO :: [HBuilderIO a b] -> HBuilderIO a [b]
joinHBuilderIO hs = HBuilderIO { hbInputIO  = \x -> mapM_ (flip hbInputIO x) hs
                               , hbOutputIO = mapM hbOutputIO hs
                               }

-- | Join list of builders into one builders
joinHBuilderIOList :: [HBuilderIO a [b]] -> HBuilderIO a [b]
joinHBuilderIOList = fmap concat . joinHBuilderIO

treeHBuilderIO :: [HBuilderIO a b -> HBuilderIO a' b'] -> HBuilderIO a b -> HBuilderIO a' [b']
treeHBuilderIO fs h = joinHBuilderIO $ map ($ h) fs

----------------------------------------------------------------
-- Stateless 
----------------------------------------------------------------

-- | Stateless histogram builder
newtype HBuilder a b = HBuilder { toBuilderST :: (forall s . ST s (HBuilderST s a b)) }

instance HistBuilder (HBuilder) where
    modifyIn  f (HBuilder h) = HBuilder (modifyIn  f <$> h)
    addCut    f (HBuilder h) = HBuilder (addCut    f <$> h)
    modifyMaybe (HBuilder h) = HBuilder (modifyMaybe <$> h)
    modifyOut f (HBuilder h) = HBuilder (modifyOut f <$> h)

instance Functor (HBuilder a) where
    fmap = modifyOut

-- | Join list of builders
joinHBuilder :: [HBuilder a b] -> HBuilder a [b]
joinHBuilder hs = HBuilder (joinHBuilderST <$> mapM toBuilderST hs)

-- | Join list of builders
joinHBuilderList :: [HBuilder a [b]] -> HBuilder a [b]
joinHBuilderList = modifyOut concat . joinHBuilder

treeHBuilder :: [HBuilder a b -> HBuilder a' b'] -> HBuilder a b -> HBuilder a' [b']
treeHBuilder fs h = joinHBuilder $ map ($ h) fs

----------------------------------------------------------------
-- Conversions
----------------------------------------------------------------

-- | Convert ST base builder to IO based one
builderSTtoIO :: HBuilderST RealWorld a b -> HBuilderIO a b
builderSTtoIO (HBuilderST i o) = HBuilderIO (stToIO . i) (stToIO o)

-- | Convert stateless builder to IO based one
toBuilderIO :: HBuilder a b -> IO (HBuilderIO a b)
toBuilderIO h = builderSTtoIO `fmap` stToIO (toBuilderST h)

----------------------------------------------------------------
-- Actual filling of histograms
----------------------------------------------------------------

fillBuilder :: HBuilder a b -> [a] -> b
fillBuilder hb xs = 
    runST $ do h <- toBuilderST hb
               mapM_ (feedOne h) xs
               freezeHBuilderST h
  
----------------------------------------------------------------
-- Histogram constructors
----------------------------------------------------------------

-- | Create histogram builder which take single item as input. Each
--   item has weight 1.
mkHist1 :: (Bin bin, Unbox val, Num val) =>
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
mkHist :: (Bin bin, Unbox val, Num val) =>
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
mkHistMaybe :: (Bin bin, Unbox val, Num val) =>
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
mkHistWgh1 :: (Bin bin, Unbox val, Num val) =>
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
mkHistWgh :: (Bin bin, Unbox val, Num val) => 
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
mkHistWghMaybe :: (Bin bin, Unbox val, Num val) => 
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
mkHistMonoid1 :: (Bin bin, Unbox val, Monoid val) =>
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
mkHistMonoid :: (Bin bin, Unbox val, Monoid val) =>
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
mkHistMonoidMaybe :: (Bin bin, Unbox val, Monoid val) =>
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
