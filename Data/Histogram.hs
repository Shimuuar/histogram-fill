{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Histogram ( Histogram(..)
                      , histBin
                      , asList
                      , module Data.Histogram.Bin
                      ) where

import Data.Array.Vector
import Data.Histogram.Bin

data Histogram bin a where
    Histogram :: (Bin bin, UA a, Num a) => 
                 bin
              -> (a,a)
              -> UArr a
              -> Histogram bin a

instance (Show a, Show (BinValue bin)) => Show (Histogram bin a) where
    show h@(Histogram bin (u,o) a) = unlines $ map showT $ asList h
        where
          showT (x,y) = show x ++ "\t" ++ show y

asList :: Histogram bin a -> [(BinValue bin, a)]
asList (Histogram bin _ arr) = map (fromIndex bin) [0..] `zip` fromU arr

histBin :: Histogram bin a -> bin
histBin (Histogram bin _ _) = bin

