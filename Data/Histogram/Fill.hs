{-# LANGUAGE GADTs        #-}
{-# LANGUAGE Rank2Types   #-}
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
module Data.Histogram.Fill ( -- * Type classes & wrappers
                             HBuilderCl(..)
                           , HBuilder
                           , builderList
                           , builderListWrap

                           -- * Fill routines
                           , createHistograms

                           -- * Histogram constructors 
                           , module Data.Histogram.Bin
                           , mkHist
                           , mkHist1
                           , mkHistWgh
                           , mkHistWgh1
                           , mkHistMonoid
                           , mkHistMonoid1
                           , forceInt
                           , forceDouble
                           , forceFloat
                           -- * Internals
                           , HistBuilder
                           ) where

import Control.Monad.ST (ST)
import Data.Monoid      (Monoid, mempty)

import Data.Array.Vector
import Data.Histogram
import Data.Histogram.Bin
import Data.Histogram.ST

----------------------------------------------------------------

-- | Create and fill histogram(s).
createHistograms :: Monoid b =>
                    HBuilder a b -- ^ Instructions how to fill histograms
                 -> [a]          -- ^ List of data to fill histogram with
                 -> b            -- ^ Result
createHistograms h xs = fillHistograms (runBuilder h) xs

----------------------------------------------------------------

-- | Histogram builder typeclass. Instance of this class contain
--   instructions how to build histograms.
class HBuilderCl h where 
    -- | Convert input type of histogram from a to a'
    modifyIn  :: (a' -> a) -> h a b -> h a' b 
    -- | Add cut to histogram. Only put value histogram if condition is true.
    addCut    :: (a -> Bool) -> h a b -> h a b
    -- | Convert output of histogram 
    modifyOut :: (b -> b') -> h a b -> h a  b'
    -- | Create stateful histogram from instructions. Histograms could
    --   be filled either in the ST monad or with createHistograms
    runBuilder :: h a b -> ST s (Accum s a b)


----------------------------------------------------------------

-- | Abstract histogram builder. All real builders should be wrapper
--   in this type
data HBuilder a b where
    MkHBuilder :: HBuilderCl h => h a b -> HBuilder a b

instance HBuilderCl HBuilder where 
    modifyIn  f (MkHBuilder h) = MkHBuilder $ modifyIn f h
    addCut    f (MkHBuilder h) = MkHBuilder $ addCut f h
    modifyOut g (MkHBuilder h) = MkHBuilder $ modifyOut g h 
    runBuilder  (MkHBuilder h) = runBuilder h


----------------------------------------------------------------

-- List of histograms. 
newtype HBuilderList a b = HBuilderList [HBuilder a b]

-- | Wrap list of histogram builders into HBuilder.
builderList :: [HBuilder a b] -> HBuilder a [b]
builderList = MkHBuilder . modifyOut (:[]) . HBuilderList

-- | Wrap list of histogram builders into HBuilder and do not change return type.
builderListWrap :: [HBuilder a b] -> HBuilder a b
builderListWrap = MkHBuilder . HBuilderList

instance HBuilderCl HBuilderList where
    modifyIn  f (HBuilderList l) = HBuilderList $ map (modifyIn f) l
    addCut    f (HBuilderList l) = HBuilderList $ map (addCut f) l
    modifyOut g (HBuilderList l) = HBuilderList $ map (modifyOut g) l
    runBuilder (HBuilderList l)  = accumList $ map runBuilder l

----------------------------------------------------------------

-- | Generic histogram builder. 
data HistBuilder a b where
    HistBuilder :: (Bin bin, UA val) =>
                   bin                                                -- Bin type
                -> val                                                -- Zero element
                -> (forall s . a -> HistogramST s bin val -> ST s ()) -- Input function
                -> (Histogram bin val -> b)                           -- Output function
                -> HistBuilder a b

instance HBuilderCl HistBuilder where
    modifyIn  f (HistBuilder bin z inp out) = HistBuilder bin z (inp . f) out
    addCut cut (HistBuilder bin z inp out) = HistBuilder bin z newInp out
        where 
          newInp x = if cut x then inp x else const (return ())
    modifyOut g (HistBuilder bin z inp out) = HistBuilder bin z  inp (g . out)
    runBuilder  (HistBuilder bin z inp out) = accumHist inp out =<< newHistogramST z bin



----------------------------------------------------------------
-- Histogram constructors 
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

-- | Create histogram builder which take single item as input. Each
--   item has weight 1. To set type of bin 'force*' function could be used.
mkHist1 :: (Bin bin, UA val, Num val) =>
           bin                      -- ^ Bin information
        -> (Histogram bin val -> b) -- ^ Output function 
        -> (a -> BinValue bin)      -- ^ Input function
        -> HBuilder a b
mkHist1 bin out inp = MkHBuilder $ HistBuilder bin 0 (flip fillOne . inp) out

-- | Create histogram builder which take many items as input. Each
--   item has weight 1. To set type of bin 'force*' function could be
--   used.
mkHist :: (Bin bin, UA val, Num val) =>
          bin                      -- ^ Bin information
       -> (Histogram bin val -> b) -- ^ Output function
       -> (a -> [BinValue bin])    -- ^ Input function 
       -> HBuilder a b
mkHist bin out inp = MkHBuilder $ HistBuilder bin 0 fill out
    where
      fill a h = mapM_ (fillOne h) $ inp a

-- | Create histogram with weighted bin. Takes one item at time. 
mkHistWgh1 :: (Bin bin, UA val, Num val) =>
              bin                        -- ^ Bin information
          -> (Histogram bin val -> b)    -- ^ Output function
          -> (a -> (BinValue bin, val))  -- ^ Input function
          -> HBuilder a b
mkHistWgh1 bin out inp = MkHBuilder $ HistBuilder bin 0 (flip fillOneW . inp) out

-- | Create histogram with weighted bin. Takes many items at time.
mkHistWgh :: (Bin bin, UA val, Num val) => 
             bin                          -- ^ Bin information
          -> (Histogram bin val  -> b)    -- ^ Output function
          -> (a -> [(BinValue bin, val)]) -- ^ Input function
          -> HBuilder a b
mkHistWgh bin out inp = MkHBuilder $ HistBuilder bin 0 fill out
    where
      fill a h = mapM_ (fillOneW h) $ inp a

-- | Create histogram with monoidal bins
mkHistMonoid1 :: (Bin bin, UA val, Monoid val) =>
              bin                         -- ^ Bin information
          -> (Histogram bin val -> b)     -- ^ Output function
          -> (a -> (BinValue bin, val))   -- ^ Input function
          -> HBuilder a b
mkHistMonoid1 bin out inp = MkHBuilder $ HistBuilder bin mempty (flip fillMonoid . inp) out

-- | Create histogram with monoidal bins. Takes many items at time.
mkHistMonoid :: (Bin bin, UA val, Monoid val) =>
              bin                         -- ^ Bin information
          -> (Histogram bin val -> b)     -- ^ Output function
          -> (a -> [(BinValue bin, val)]) -- ^ Input function
          -> HBuilder a b
mkHistMonoid bin out inp = MkHBuilder $ HistBuilder bin mempty fill out
    where
      fill a h = mapM_ (fillMonoid h) $ inp a
