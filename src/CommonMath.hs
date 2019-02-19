{-# LANGUAGE ScopedTypeVariables #-}

module CommonMath where

import qualified Data.Foldable as F
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


-- functions to help tally the number of occurrences
-- of list elements and their proportions:

count :: forall t a . (Foldable t, Ord a)
    => t a -> Map a Int
count elements =
    F.foldl' addCount M.empty elements
    where
        addCount :: Map a Int -> a -> Map a Int
        addCount counter el = M.insertWith (+) el 1 counter


proportions :: (Foldable t, Ord a) => t a -> [Double]
proportions xs =
    [c / n | c' <- counts, let c = fromIntegral c']
    where
        counts = M.elems (count xs)

        n = fromIntegral $ length xs


-- different impurity measures:

gini :: (Foldable t, Ord a) => t a -> Double
gini xs =
    F.sum [p * (1-p) | p <- proportions xs]


entropy :: (Foldable t, Ord a) => t a -> Double
entropy xs =
    -F.sum [p * logBase 2 p | p <- proportions xs]


misclassification :: (Foldable t, Ord a) => t a -> Double
misclassification xs =
    1 - maximum (proportions xs)
