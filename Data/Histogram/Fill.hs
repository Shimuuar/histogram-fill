{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types   #-}
-- |
-- Module     : Data.Histogram.Fill
-- Copyright  : Copyright (c) 2009, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Stateful and pure (still stateful under the hood) accumulators. 
--
module Data.Histogram.Fill ( 
    -- * Builder type class
    HistBuilder(..)
    -- ** Operators
  , (<<-)
  , (<<-|)
  , (<<?)
  , (<<-$)
  , (-<<)
    -- * Histogram builders
    -- ** Stateful
  , HBuilderM
  , feedOne
  , freezeHBuilderM
  , treeHBuilderM
    -- ** Stateless
  , HBuilder(HBuilder)
  , toHBuilderST
  , toHBuilderIO
  , treeHBuilder
    -- * Histogram constructors
    -- ** Using unboxed vectors
  , module Data.Histogram.Bin
  , mkSimple
  , mkWeighted
  , mkMonoidal
  , mkFoldBuilder
    -- ** Using generic vectors
  , mkSimpleG
  , mkWeightedG
  , mkMonoidalG
  , mkFoldBuilderG
    -- ** Pure fold
  , mkFolder
    -- ** Generic constructors
  , mkStatefulBuilder
    -- * Fill histograms
  , fillBuilder
  , fillBuilderVec
    -- * Auxillary functions
    -- $auxillary
  , forceInt
  , forceDouble
  , forceFloat
    -- * Examples
    -- $examples
    -- * Deprecated functions
  , joinHBuilder
  , joinHBuilderM
  ) where

import Control.Applicative
import Control.Monad       (when,liftM,liftM2)
import Control.Monad.ST
import Control.Monad.Primitive

import Data.STRef
import Data.Monoid            (Monoid(..))
import Data.Vector.Unboxed    (Unbox)
import qualified Data.Vector.Generic as G
import qualified Data.Foldable       as F
import qualified Data.Traversable    as F

import Data.Histogram
import qualified Data.Histogram.Generic as H
import Data.Histogram.Bin
import Data.Histogram.ST



----------------------------------------------------------------
-- Type class
----------------------------------------------------------------

-- | Type class for stateful accumulators. In this module they are
--   called builders. Every builder is parametrized by two
--   types. First one is type of values which are fed to accumulator
--   and second one is type of values which could be extracted from
--   it.
--
--   Every instance of 'HBuilder' should be instance of 'Functor' too
--   and satisfy 'fmap' == 'modifyOut'.
class HistBuilder h where
    -- | Apply function to output of histogram.
    modifyOut     :: (b -> b') -> h a b -> h a  b'
    -- | Change input of builder by applying function to it.
    modifyIn      :: (a' -> a) -> h a b -> h a' b
    -- | Put all values in container into builder 
    fromContainer :: F.Foldable f => h a b -> h (f a) b
    -- | Add cut to histogram. Value would be putted into histogram
    --   only if condition is true. 
    addCut        :: (a -> Bool) -> h a b -> h a b


-- | Modify input of builder
(<<-) :: HistBuilder h => h a b -> (a' -> a) -> h a' b
(<<-) = flip modifyIn
{-# INLINE (<<-) #-}

-- | Modify input of builder to use composite input
(<<-|) :: (HistBuilder h, F.Foldable f) => h a b -> (a' -> f a) -> h a' b
h <<-| f = fromContainer h <<- f
{-# INLINE (<<-|) #-}

-- | Add cut for input
(<<?) :: HistBuilder h => h a b -> (a -> Bool) -> h a b
(<<?) = flip addCut
{-# INLINE (<<?) #-}

-- | Apply function which modify builder
(<<-$) :: HistBuilder h => h a b -> (h a b -> h a' b) -> h a' b
h <<-$ f = f h
{-# INLINE (<<-$) #-}

-- | Modify output of histogram. In fact it's same as '<$>' but have opposite fixity
(-<<) :: HistBuilder h => (b -> b') -> h a b -> h a b'
(-<<) = modifyOut
{-# INLINE (-<<) #-}

-- Fixity of operator
infixl 5 <<-
infixl 5 <<-|
infixl 5 <<?
infixl 5 <<-$
infixr 4 -<<


-- $examples
--
-- All examples will make use of operators to create builders. It's
-- possible to avoid their use but operators offer clear notation and
-- compose nicely in pipeline. Also note that data flows from right to
-- left as with '.' operator.
--
-- First example just counts ints in in [0..4] inclusive range.
-- 'fillBuilder' is used to put all values into accumulator.
--
-- > ghci> let h = forceInt -<< mkSimple (BinI 0 4)
-- > ghci> fillBuilder h [0,0,0,1,1,2,3,4,4,4]
-- > # Histogram
-- > # Underflows = 0
-- > # Overflows  = 0
-- > # BinI
-- > # Low  = 0
-- > # High = 4
-- > 0       3
-- > 1       2
-- > 2       1
-- > 3       1
-- > 4       3
--
-- More involved example only accept even numbers. Filtering could be
-- achieved with either 'addCut' or '<<?' operator.
--
-- > forceInt -<< mkSimple (BinI 0 4) <<? even
--
-- Although for example above same result could be acheved by
-- filtering of input it doesn't work when multiple histograms with
-- different cuts are filled simultaneously.
--
-- Next example illustrate use of applicative interface. Here two
-- histograms are filled at the same time. First accept only even
-- numbers and second only odd ones. Results are put into the tuple.
--
-- > (,) <$> 
-- >   (forceInt -<< mkSimple (BinI 0 4) <<? even)
-- >   (forceInt -<< mkSimple (BinI 0 4) <<? odd)
--
-- Another approach is to use 'F.sequenceA' to simultaneously fill
-- list (or any other 'Travesable'). 
--
-- > Data.Traversable.sequenceA [
-- >     forceInt -<< mkSimple (BinI 0 4) <<? even
-- >   , forceInt -<< mkSimple (BinI 0 4) <<? odd
-- >   ]
--
-- If one wants to collect result from many histograms he can take an
-- advantage of 'Monoid' instance of 'HBuilder'. Example below
-- concatenates string outputs of individual histograms.
--
-- > mconcat [
-- >     show . forceInt -<< mkSimple (BinI 0 4) <<? even
-- >   , show . forceInt -<< mkSimple (BinI 0 4) <<? odd
-- >   ]


----------------------------------------------------------------
-- Monadic builder
----------------------------------------------------------------

-- | Stateful histogram builder. Adding value to builder could be done
--   with 'feedOne' and result could be extracted with
--   'freezeHBuilderM'.
--
--   There are two ways to obtain stateful builder. First and
--   recommended is to thaw 'HBuilder' using 'toHBuilderIO' or
--   'toHBuilderST'. Second is to use 'mkStatefulBuilder'.
data HBuilderM m a b = HBuilderM { hbInput  :: a -> m ()
                                 , hbOutput :: m b
                                 }

-- | Builders modified using 'HistBuilder' API will share same buffer.
instance PrimMonad m => HistBuilder (HBuilderM m) where
    modifyIn    f h = h { hbInput  = hbInput h . f }
    addCut      f h = h { hbInput  = \x -> when (f x) (hbInput h x) }
    fromContainer h = h { hbInput  = F.mapM_ (hbInput h) }
    modifyOut   f h = h { hbOutput = f `liftM` hbOutput h }

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

instance (PrimMonad m, Monoid b) => Monoid (HBuilderM m a b) where
    mempty = HBuilderM { hbInput  = \_ -> return ()
                       , hbOutput = return mempty
                       }
    mappend h1 h2 = mappend <$> h1 <*> h2
    mconcat = fmap mconcat . F.sequenceA
    {-# INLINE mempty  #-}
    {-# INLINE mconcat #-}


-- | Put one item into histogram
feedOne :: PrimMonad m => HBuilderM m a b -> a -> m ()
feedOne = hbInput
{-# INLINE feedOne #-}

-- | Extract result from histogram builder. It's safe to call this
--   function multiple times and mutate builder afterwards.
freezeHBuilderM :: PrimMonad m => HBuilderM m a b -> m b
freezeHBuilderM = hbOutput
{-# INLINE freezeHBuilderM #-}

-- | Join histogram builders in container
joinHBuilderM :: (F.Traversable f, PrimMonad m) => f (HBuilderM m a b) -> HBuilderM m a (f b)
joinHBuilderM hs = HBuilderM { hbInput  = \x -> F.mapM_ (flip hbInput x) hs
                             , hbOutput = F.mapM hbOutput hs
                             }
{-# INLINE     joinHBuilderM #-}
{-# DEPRECATED joinHBuilderM "Use Data.Traversable.sequenceA instead" #-}

-- | Apply functions to builder
treeHBuilderM :: (PrimMonad m, F.Traversable f) => f (HBuilderM m a b -> HBuilderM m a' b') -> HBuilderM m a b -> HBuilderM m a' (f b')
treeHBuilderM fs h = F.sequenceA $ fmap ($ h) fs
{-# INLINE treeHBuilderM #-}



----------------------------------------------------------------
-- Stateless
----------------------------------------------------------------

-- | Wrapper around stateful histogram builder. It is much more
--   convenient to work with than 'HBuilderM'.
newtype HBuilder a b = HBuilder (forall s . ST s (HBuilderM (ST s) a b))

-- | Convert builder to stateful builder in ST monad
toHBuilderST :: HBuilder a b -> ST s (HBuilderM (ST s) a b)
{-# INLINE toHBuilderST #-}
toHBuilderST (HBuilder hb) = hb

-- | Convert builder to builder in IO monad
toHBuilderIO :: HBuilder a b -> IO (HBuilderM IO a b)
{-# INLINE toHBuilderIO #-}
toHBuilderIO (HBuilder h) = do
  builder <- stToIO h
  return (HBuilderM
          (stToIO . hbInput builder)
          (stToIO $ hbOutput builder))

instance HistBuilder (HBuilder) where
    modifyIn    f (HBuilder h) = HBuilder (modifyIn  f <$> h)
    addCut      f (HBuilder h) = HBuilder (addCut    f <$> h)
    fromContainer (HBuilder h) = HBuilder (fromContainer <$> h)
    modifyOut   f (HBuilder h) = HBuilder (modifyOut f <$> h)

instance Functor (HBuilder a) where
    fmap = modifyOut
instance Applicative (HBuilder a) where
    pure x  = HBuilder (return $ pure x)
    (HBuilder f) <*> (HBuilder g) = HBuilder $ liftM2 (<*>) f g
instance Monoid b => Monoid (HBuilder a b) where
    mempty      = HBuilder (return mempty)
    mappend h g = mappend <$> h <*> g
    mconcat     = fmap mconcat . F.sequenceA
    {-# INLINE mempty  #-}
    {-# INLINE mappend #-}
    {-# INLINE mconcat #-}

-- | Join hitogram builders in container.
joinHBuilder :: F.Traversable f => f (HBuilder a b) -> HBuilder a (f b)
joinHBuilder hs = HBuilder (joinHBuilderM <$> F.mapM toHBuilderST hs)
{-# INLINE     joinHBuilder #-}
{-# DEPRECATED joinHBuilder "Use Data.Traversable.sequenceA instead" #-}

-- | Apply function to builder
treeHBuilder :: F.Traversable f => f (HBuilder a b -> HBuilder a' b') -> HBuilder a b -> HBuilder a' (f b')
treeHBuilder fs h = F.sequenceA $ fmap ($ h) fs
{-# INLINE treeHBuilder #-}



----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

-- | Create builder. Bin content will be incremented by 1 for each
--   item put into histogram
mkSimple :: (Bin bin, Unbox val, Num val
            ) => bin -> HBuilder (BinValue bin) (Histogram bin val)
mkSimple = mkSimpleG
{-# INLINE mkSimple #-}

-- | Create builder. Bin content will incremented by weight supplied
--   for each item put into histogram
mkWeighted :: (Bin bin, Unbox val, Num val
              ) => bin -> HBuilder (BinValue bin,val) (Histogram bin val)
mkWeighted = mkWeightedG
{-# INLINE mkWeighted #-}

-- | Create builder. New value wil be mappended to current content of
--   a bin for each item put into histogram
mkMonoidal :: (Bin bin, Unbox val, Monoid val
              ) => bin -> HBuilder (BinValue bin,val) (Histogram bin val)
mkMonoidal = mkMonoidalG
{-# INLINE mkMonoidal #-}

-- | Create most generic histogram builder.
mkFoldBuilder :: (Bin bin, Unbox val)
              => bin               -- ^ Binning algorithm
              -> val               -- ^ Initial value
              -> (val -> a -> val) -- ^ Folding function
              -> HBuilder (BinValue bin, a) (Histogram bin val)
{-# INLINE mkFoldBuilder #-}
mkFoldBuilder = mkFoldBuilderG



-- | Create builder. Bin content will be incremented by 1 for each
--   item put into histogram
mkSimpleG :: (Bin bin, G.Vector v val, Num val
            ) => bin -> HBuilder (BinValue bin) (H.Histogram v bin val)
mkSimpleG bin = HBuilder $ do
  acc <- newMHistogram 0 bin
  return HBuilderM { hbInput  = \x -> fill acc x (+) 1
                   , hbOutput = freezeHist acc
                   }
{-# INLINE mkSimpleG #-}

-- | Create builder. Bin content will incremented by weight supplied
--   for each item put into histogram
mkWeightedG :: (Bin bin, G.Vector v val, Num val
              ) => bin -> HBuilder (BinValue bin,val) (H.Histogram v bin val)
mkWeightedG bin = mkFoldBuilderG bin 0 (+)
{-# INLINE mkWeightedG #-}

-- | Create builder. New value wil be mappended to current content of
--   a bin for each item put into histogram
mkMonoidalG :: (Bin bin, G.Vector v val, Monoid val
              ) => bin -> HBuilder (BinValue bin,val) (H.Histogram v bin val)
mkMonoidalG bin = mkFoldBuilderG bin mempty mappend
{-# INLINE mkMonoidalG #-}

-- | Create most generic histogram builder.
mkFoldBuilderG :: (Bin bin, G.Vector v val)
               => bin               -- ^ Binning algorithm
               -> val               -- ^ Initial value
               -> (val -> a -> val) -- ^ Folding function
               -> HBuilder (BinValue bin, a) (H.Histogram v bin val)
{-# INLINE mkFoldBuilderG #-}
mkFoldBuilderG bin x0 f = HBuilder $ do
  acc <- newMHistogram x0 bin
  return HBuilderM { hbInput  = \(!x,!w) -> fill acc x f w
                   , hbOutput = freezeHist acc
                   }


-- | Create histogram builder which just does ordinary pure fold. It
--   is intended for use when some fold should be performed together
--   with histogram filling
mkFolder :: b -> (a -> b -> b) -> HBuilder a b
{-# INLINE mkFolder #-}
mkFolder a f = HBuilder $ do
  ref <- newSTRef a
  return HBuilderM { hbInput  = \x -> do acc <- readSTRef ref
                                         let !acc' = f x acc
                                         writeSTRef ref acc'
                   , hbOutput = readSTRef ref
                   }


-- | Create stateful histogram builder. Output function should be safe
--   to call multiple times and builder could be modified afterwards.
--   So functions like @unsafeFreeze@ from @vector@ couldn't be used.
mkStatefulBuilder :: PrimMonad m
                  => (a -> m ()) -- ^ Add value to accumulator
                  -> m b         -- ^ Extract result from accumulator
                  -> HBuilderM m a b
{-# INLINE mkStatefulBuilder #-}
mkStatefulBuilder = HBuilderM



----------------------------------------------------------------
-- Actual filling of histograms
----------------------------------------------------------------

-- | Fill histogram builder.
fillBuilder :: F.Foldable f => HBuilder a b -> f a -> b
fillBuilder hb xs =
    runST $ do h <- toHBuilderST hb
               F.mapM_ (feedOne h) xs
               freezeHBuilderM h

-- | Fill histogram builder.
fillBuilderVec :: G.Vector v a => HBuilder a b -> v a -> b
fillBuilderVec hb vec =
    runST $ do h <- toHBuilderST hb
               G.mapM_ (feedOne h) vec
               freezeHBuilderM h

----------------------------------------------------------------

-- $auxillary
--
-- In some cases builder constructors do not constrain output type
-- enough. Output type is still parametric in value type of histogram.
-- Functions below are just 'id' function with more restrictive
-- signature.
--
-- In example below 'forceInt' used to fix type of histogram to
-- 'Histogram BinI Int'. Without it compiler cannot infer type of
-- intermediate histogram.
--
-- > show . forceInt -<< mkSimple (BinI 1 10)

forceInt :: H.Histogram v bin Int -> H.Histogram v bin Int
forceInt = id

forceDouble :: H.Histogram v bin Double -> H.Histogram v bin Double
forceDouble = id

forceFloat :: H.Histogram v bin Float -> H.Histogram v bin Float
forceFloat = id
