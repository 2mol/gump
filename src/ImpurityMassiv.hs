{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ImpurityMassiv where

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A

tally ::
     (Mutable r Ix1 e, Resize r ix, Ord e, Load r ix e)
  => Array r ix e
  -> Array DS Ix1 (e, Int)
tally arr
  | isEmpty arr = A.empty
  | otherwise = A.mapMaybeS id $ A.unfoldrN (sz + 1) count (0, 0, sorted ! 0)
  where
    sz@(Sz k) = size sorted
    count (!i, !n, !prev)
      | i < k =
        let !e' = A.unsafeLinearIndex sorted i
         in if prev == e'
              then Just (Nothing, (i + 1, n + 1, prev))
              else Just (Just (prev, n), (i + 1, 1, e'))
      | otherwise = Just (Just (prev, n), (i + 1, n, prev))
    {-# INLINE count #-}
    sorted = A.quicksort $ flatten arr
{-# INLINE tally #-}

proportions ::
     (Mutable r Ix1 a, Resize r ix, Ord a, Load r ix a)
  => Array r ix a
  -> Array D Ix1 Double
proportions arr = A.map (\c -> fromIntegral c / n) counts
  where
    counts = computeAs P (snd <$> tally arr)
    n = fromIntegral $ unSz $ size counts


entropy ::
     (Mutable r Ix1 a, Resize r ix, Ord a, Load r ix a)
  => Array r ix a
  -> Double
entropy = negate . A.sum . A.map (\p -> p * logBase 2 p) . proportions



gini ::
     (Mutable r Ix1 a, Resize r ix, Ord a, Load r ix a)
  => Array r ix a
  -> Double
gini = A.sum . A.map (\p -> p * (1 - p)) . proportions


-- misclassification coefficient as an impurity measure

misclassification ::
     (Mutable r Ix1 a, Resize r ix, Ord a, Load r ix a)
  => Array r ix a
  -> Double
misclassification xs = 1 - maximum' (proportions xs)
