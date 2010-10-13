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
                           , HBuilderM
                           , feedOne
                           , freezeHBuilderM
                           , joinHBuilderM
                           , joinHBuilderMonoidM
                           , treeHBuilderM
                           , treeHBuilderMonoidM
                             -- ** Stateless
                           , HBuilder
                           , joinHBuilder
                           , joinHBuilderMonoid
                           , treeHBuilder
                           , treeHBuilderMonoid
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

import Control.Applicative
import Control.Monad       (when,liftM,liftM2)
import Control.Monad.ST 
import Control.Monad.Primitive

import Data.Monoid         (Monoid(..))
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
data HBuilderM m a b = HBuilderM { hbInput  :: a -> m ()
                                 , hbOutput :: m b
                                 }

instance PrimMonad m => HistBuilder (HBuilderM m) where
    modifyIn  f h = h { hbInput  = hbInput h . f }
    addCut    f h = h { hbInput  = \x -> when (f x) (hbInput h x) }
    modifyMaybe h = h { hbInput  = modified } 
        where modified (Just x) = hbInput h x
              modified Nothing  = return ()
    modifyOut f h = h { hbOutput = f `liftM` hbOutput h }

instance PrimMonad m => Functor (HBuilderM m a) where
    fmap = modifyOut
instance PrimMonad m => Applicative (HBuilderM m a) where
    pure x = HBuilderM { hbInput  = const $ return ()
                       , hbOutput = return x
                       }
    f <*> g = HBuilderM { hbInput  = \a -> hbInput f a >> hbInput g a
                        , hbOutput = do a <- hbOutput f
                                        b <- hbOutput g
                                        return (a b)
                        }
                                        

-- | Put one value into histogram
feedOne :: PrimMonad m => HBuilderM m a b -> a -> m ()
feedOne = hbInput
{-# INLINE feedOne #-}

-- | Create stateful histogram from instructions. Histograms could
--   be filled either in the ST monad or with createHistograms
freezeHBuilderM :: PrimMonad m => HBuilderM m a b -> m b
freezeHBuilderM = hbOutput
{-# INLINE freezeHBuilderM #-}

-- | Join list of builders into one builder
joinHBuilderM :: PrimMonad m => [HBuilderM m a b] -> HBuilderM m a [b]
joinHBuilderM hs = HBuilderM { hbInput  = \x -> mapM_ (flip hbInput x) hs
                             , hbOutput = mapM hbOutput hs
                             }
{-# INLINE joinHBuilderM #-}

-- | Join list of builders into one builders
joinHBuilderMonoidM :: (PrimMonad m, Monoid b) => [HBuilderM m a b] -> HBuilderM m a b
joinHBuilderMonoidM = fmap mconcat . joinHBuilderM
{-# INLINE joinHBuilderMonoidM #-}

treeHBuilderM :: PrimMonad m => [HBuilderM m a b -> HBuilderM m a' b'] -> HBuilderM m a b -> HBuilderM m a' [b']
treeHBuilderM fs h = joinHBuilderM $ map ($ h) fs
{-# INLINE treeHBuilderM #-}

treeHBuilderMonoidM :: (PrimMonad m, Monoid b') => 
                        [HBuilderM m a b -> HBuilderM m a' b'] -> HBuilderM m a b -> HBuilderM m a' b'
treeHBuilderMonoidM fs h = joinHBuilderMonoidM $ map ($ h) fs
{-# INLINE treeHBuilderMonoidM #-}


----------------------------------------------------------------
-- Stateless 
----------------------------------------------------------------

-- | Stateless histogram builder
newtype HBuilder a b = HBuilder { toBuilderM :: (forall s . ST s (HBuilderM (ST s) a b)) }

instance HistBuilder (HBuilder) where
    modifyIn  f (HBuilder h) = HBuilder (modifyIn  f <$> h)
    addCut    f (HBuilder h) = HBuilder (addCut    f <$> h)
    modifyMaybe (HBuilder h) = HBuilder (modifyMaybe <$> h)
    modifyOut f (HBuilder h) = HBuilder (modifyOut f <$> h)

instance Functor (HBuilder a) where
    fmap = modifyOut
instance Applicative (HBuilder a) where
    pure x  = HBuilder (return $ pure x)
    (HBuilder f) <*> (HBuilder g) = HBuilder $ liftM2 (<*>) f g 

-- | Join list of builders
joinHBuilder :: [HBuilder a b] -> HBuilder a [b]
joinHBuilder hs = HBuilder (joinHBuilderM <$> mapM toBuilderM hs)
{-# INLINE joinHBuilder #-}

-- | Join list of builders
joinHBuilderMonoid :: Monoid b => [HBuilder a b] -> HBuilder a b
joinHBuilderMonoid = modifyOut mconcat . joinHBuilder
{-# INLINE joinHBuilderMonoid #-}

treeHBuilder :: [HBuilder a b -> HBuilder a' b'] -> HBuilder a b -> HBuilder a' [b']
treeHBuilder fs h = joinHBuilder $ map ($ h) fs
{-# INLINE treeHBuilder #-}

treeHBuilderMonoid :: Monoid b' => [HBuilder a b -> HBuilder a' b'] -> HBuilder a b -> HBuilder a' b'
treeHBuilderMonoid fs h = joinHBuilderMonoid $ map ($ h) fs
{-# INLINE treeHBuilderMonoid #-}

----------------------------------------------------------------
-- Actual filling of histograms
----------------------------------------------------------------

fillBuilder :: HBuilder a b -> [a] -> b
fillBuilder hb xs = 
    runST $ do h <- toBuilderM hb
               mapM_ (feedOne h) xs
               freezeHBuilderM h


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
  return $ HBuilderM  { hbInput  = fillOne acc . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }
{-# INLINE mkHist1 #-}

-- | Create histogram builder which take many items as input. Each
--   item has weight 1.
mkHist :: (Bin bin, Unbox val, Num val) =>
          bin                      -- ^ Bin information
       -> (Histogram bin val -> b) -- ^ Output function
       -> (a -> [BinValue bin])    -- ^ Input function 
       -> HBuilder a b
mkHist bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderM  { hbInput  = mapM_ (fillOne acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }
{-# INLINE mkHist #-}

-- | Create histogram builder which at most one item as input. Each
--   item has weight 1. 
mkHistMaybe :: (Bin bin, Unbox val, Num val) =>
          bin                         -- ^ Bin information
       -> (Histogram bin val -> b)    -- ^ Output function
       -> (a -> Maybe (BinValue bin)) -- ^ Input function 
       -> HBuilder a b
mkHistMaybe bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderM  { hbInput  = maybe (return ()) (fillOne acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }
{-# INLINE mkHistMaybe #-}

-- | Create histogram with weighted bin. Takes one item at time. 
mkHistWgh1 :: (Bin bin, Unbox val, Num val) =>
              bin                        -- ^ Bin information
          -> (Histogram bin val -> b)    -- ^ Output function
          -> (a -> (BinValue bin, val))  -- ^ Input function
          -> HBuilder a b
mkHistWgh1 bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderM  { hbInput  = fillOneW acc . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }
{-# INLINE mkHistWgh1 #-}

-- | Create histogram with weighted bin. Takes many items at time.
mkHistWgh :: (Bin bin, Unbox val, Num val) => 
             bin                          -- ^ Bin information
          -> (Histogram bin val  -> b)    -- ^ Output function
          -> (a -> [(BinValue bin, val)]) -- ^ Input function
          -> HBuilder a b
mkHistWgh bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderM  { hbInput  = mapM_ (fillOneW acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }
{-# INLINE mkHistWgh #-}

-- | Create histogram with weighted bin. Takes many items at time.
mkHistWghMaybe :: (Bin bin, Unbox val, Num val) => 
                  bin                              -- ^ Bin information
               -> (Histogram bin val  -> b)        -- ^ Output function
               -> (a -> Maybe (BinValue bin, val)) -- ^ Input function
               -> HBuilder a b
mkHistWghMaybe bin out inp = HBuilder $ do
  acc <- newMHistogram 0 bin
  return $ HBuilderM  { hbInput  = maybe (return ()) (fillOneW acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }
{-# INLINE mkHistWghMaybe #-}

-- | Create histogram with monoidal bins
mkHistMonoid1 :: (Bin bin, Unbox val, Monoid val) =>
              bin                         -- ^ Bin information
          -> (Histogram bin val -> b)     -- ^ Output function
          -> (a -> (BinValue bin, val))   -- ^ Input function
          -> HBuilder a b
mkHistMonoid1 bin out inp = HBuilder $ do
  acc <- newMHistogram mempty bin
  return $ HBuilderM  { hbInput  = fillMonoid acc . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }
{-# INLINE mkHistMonoid1 #-}

-- | Create histogram with monoidal bins. Takes many items at time.
mkHistMonoid :: (Bin bin, Unbox val, Monoid val) =>
              bin                         -- ^ Bin information
          -> (Histogram bin val -> b)     -- ^ Output function
          -> (a -> [(BinValue bin, val)]) -- ^ Input function
          -> HBuilder a b
mkHistMonoid bin out inp = HBuilder $ do
  acc <- newMHistogram mempty bin
  return $ HBuilderM  { hbInput  = mapM_ (fillMonoid acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }
{-# INLINE mkHistMonoid #-}

-- | Create histogram with monoidal bins
mkHistMonoidMaybe :: (Bin bin, Unbox val, Monoid val) =>
                     bin                              -- ^ Bin information
                  -> (Histogram bin val -> b)         -- ^ Output function
                  -> (a -> Maybe (BinValue bin, val)) -- ^ Input function
                  -> HBuilder a b
mkHistMonoidMaybe bin out inp = HBuilder $ do
  acc <- newMHistogram mempty bin
  return $ HBuilderM  { hbInput  = maybe (return ()) (fillMonoid acc) . inp
                      , hbOutput = fmap out (freezeHist acc)
                      }
{-# INLINE mkHistMonoidMaybe #-}

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
