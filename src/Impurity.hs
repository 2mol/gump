{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Impurity where

import           Data.Foldable as F
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- count the number of occurrences of every list element
tally :: forall t a . (Foldable t, Ord a) => t a -> Map a Int
tally elements =
    F.foldr' addCount M.empty elements
    where
        addCount :: a -> Map a Int -> Map a Int
        addCount !el !counter = M.insertWith (+) el 1 counter
        {-# INLINE addCount #-}
{-# INLINE tally #-}

tallyM :: forall t a . (Foldable t, Ord a) => t a -> [(a, Int)]
tallyM = M.toList . tally


-- calculate which element occurs which percentage of the time
proportions :: (Foldable t, Ord a) => t a -> [Double]
proportions xs =
    [c / n | c' <- counts, let c = fromIntegral c']
    where
        counts = M.elems (tally xs)
        n = fromIntegral $ length xs
{-# INLINE proportions #-}


-- entropy measure
-- https://en.wikipedia.org/wiki/Entropy#Statistical_mechanics
entropy :: (Foldable t, Ord a) => t a -> Double
entropy xs =
    -Prelude.sum [p * logBase 2 p | p <- proportions xs]
{-# INLINE entropy #-}


-- gini coefficient as an impurity measure
gini :: (Foldable t, Ord a) => t a -> Double
gini xs =
    Prelude.sum [p * (1 - p) | p <- proportions xs]


-- misclassification coefficient as an impurity measure
misclassification :: (Foldable t, Ord a) => t a -> Double
misclassification xs =
    1 - Prelude.maximum (proportions xs)
