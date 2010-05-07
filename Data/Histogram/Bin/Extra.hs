{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module     : Data.Histogram.Bin
-- Copyright  : Copyright (c) 2010, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Extra binning algorithms

module Data.Histogram.Bin.Extra ( BinPermute(permutedBin, permuteTo, permuteFrom)
                                , permuteBin
                                ) where

import Control.Applicative
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Vector.Unboxed ((!))
import Text.Read

import Data.Histogram.Bin
import Data.Histogram.Parse

-- | Direct permutation of indices. 
data BinPermute b = BinPermute { permutedBin :: b
                               , permuteTo   :: U.Vector Int
                               , permuteFrom :: U.Vector Int
                               }
instance Bin b => Bin (BinPermute b) where
    type BinValue (BinPermute b) = BinValue b
    toIndex   (BinPermute b to _)   x = to ! toIndex b x
    fromIndex (BinPermute b _ from) i = fromIndex b (from ! i)
    inRange   (BinPermute b _ _) x = inRange b x
    nBins     (BinPermute b _ _) = nBins b

instance Show b => Show (BinPermute b) where
    show (BinPermute b to _) = unlines [ "# BinPermute"
                                       , "# Permutation = " ++ show (U.toList to)
                                       ] ++ show b

instance Read BinI => Read (BinPermute BinI) where
    readPrec = do keyword "BinPermute"
                  to   <- U.fromList <$> value "Permutation"
                  from <- case checkPermutation (invertPermutation to) of
                            Just v  -> return v
                            Nothing -> fail "Invalid permutation"
                  b  <- readPrec 
                  return $ BinPermute b to from

-- Check whether this viable permutation
checkPermutation :: U.Vector Int -> Maybe (U.Vector Int)
checkPermutation v | U.any bad v = Nothing
                   | otherwise   = Just v
                   where
                     n     = U.length v
                     bad i = i < 0 || i >= n

-- Calculate inverse permutation                     
invertPermutation :: U.Vector Int -> U.Vector Int
invertPermutation v = U.create $ do a <- M.newWith n (-1)
                                    forM_ [0..n-1] (writeInvert a)
                                    return a
  where
    n = U.length v
    writeInvert a i | j >= 0 && j < n = M.write a j i
                    | otherwise       = return ()
                    where j = v ! i

-- | Constuct bin permutation from function.
permuteBin :: Bin b => (Int -> Int) -> b -> Maybe (BinPermute b)
permuteBin f b = BinPermute b <$> checkPermutation to <*> checkPermutation (invertPermutation to)
    where
      to   = U.map f $ U.enumFromN 0 (nBins b)
