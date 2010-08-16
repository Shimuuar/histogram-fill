{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Histogram.Bin.Indexable (
    -- * Indexable bins
    Indexable(..)
  , BinIx
  , binIx
    -- * 2D indexable bins
  , Indexable2D(..)
  , BinIx2D
  , binIx2D
  ) where

import Control.Monad (liftM2)
import Data.Histogram.Bin
import Data.Histogram.Parse
import Text.Read (Read(..))


-- | Indexable is value which could be converted to and from Int
-- without information loss.
--
-- Always true
--
-- > deindex . index = id
--
-- Only if Int is in range
--
-- > index . deindex = id
class Indexable a where
    -- | Convert value to index
    index :: a -> Int 
    -- | Convert index to value
    deindex :: Int -> a

-- | This type class is same as Indexable but for 2D values.
class Indexable2D a where
    -- | Convert value to index
    index2D :: a -> (Int,Int)
    -- | Convert index to value
    deindex2D :: (Int,Int) -> a

instance (Indexable a, Indexable b) => Indexable2D (a,b) where
    index2D   (x,y) = (index x,   index y)
    deindex2D (i,j) = (deindex i, deindex j)

----------------------------------------------------------------
-- Bins for indexables
----------------------------------------------------------------

-- | Binning for indexable values
newtype BinIx i = BinIx BinI
                  deriving Eq

-- | Construct indexed bin
binIx :: Indexable i => i -> i -> BinIx i
binIx lo hi = BinIx $ BinI (index lo) (index hi)

instance Indexable i => Bin (BinIx i) where
    type BinValue (BinIx i) = i
    toIndex   (BinIx b) x = toIndex b (index x)
    fromIndex (BinIx b) i = deindex (fromIndex b i)
    inRange   (BinIx b) x = inRange b (index x)
    nBins (BinIx b) = nBins b

instance (Show i, Indexable i) => Show (BinIx i) where
    show (BinIx (BinI lo hi)) = unlines [ "# BinIx"
                                        , "# Low  = " ++ show (deindex lo :: i)
                                        , "# High = " ++ show (deindex hi :: i)
                                        ]
instance (Read i, Indexable i) => Read (BinIx i) where
    readPrec = keyword "BinIx" >> liftM2 binIx (value "Low") (value "High")

----------------------------------------------------------------
-- Indexed 2D bins
----------------------------------------------------------------
-- | Binning for 2D indexable value
newtype BinIx2D i = BinIx2D (Bin2D BinI BinI)

-- | Construct indexed bin
binIx2D :: Indexable2D i => i -> i -> BinIx2D i
binIx2D lo hi = let (ix,iy) = index2D lo
                    (jx,jy) = index2D hi
                in BinIx2D $ BinI ix jx >< BinI iy jy

instance Indexable2D i => Bin (BinIx2D i) where
    type BinValue (BinIx2D i) = i
    toIndex   (BinIx2D b) x = toIndex b (index2D x)
    fromIndex (BinIx2D b) i = deindex2D $ fromIndex b i
    inRange   (BinIx2D b) x = inRange b (index2D x)
    nBins     (BinIx2D b)   = nBins b

instance (Show i, Indexable2D i) => Show (BinIx2D i) where
    show (BinIx2D b) = unlines [ "# BinIx2D"
                               , "# Low  = " ++ show (deindex2D (fromIndex b 0            ) :: i)
                               , "# High = " ++ show (deindex2D (fromIndex b (nBins b - 1)) :: i)
                               ]
instance (Read i, Indexable2D i) => Read (BinIx2D i) where
    readPrec = do
      keyword "BinIx2D"
      l <- value "Low"
      h <- value "High"
      return $ binIx2D l h
