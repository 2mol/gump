{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ID3Massiv where

{-
Description from [https://scikit-learn.org/stable/modules/tree.html]:

ID3 (Iterative Dichotomiser 3) was developed in 1986 by Ross Quinlan. The algorithm creates a
multiway tree, finding for each node (i.e. in a greedy manner) the categorical feature that will
yield the largest information gain for categorical targets. Trees are grown to their maximum size
and then a pruning step is usually applied to improve the ability of the tree to generalise to
unseen data.
-}

import qualified Data.Foldable as F
import           Data.Function ((&))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Massiv.Array as A

import           Impurity (entropy)

{-
how to build a decision tree:

0. put data matrix values in buckets.
   -> ideally Int, otherwise Double. (Keep dict of that mapping.)
1. get information gains for each column.
2. see which column has the highest gain.
3. perform `groupBy` on matrix (and feature vector) by that column.
4. recursively repeat with those smaller matrices in each group.
5. the keys are the tree nodes.

-}

-- generic groupBy
groupByColumn :: forall e . (Unbox e, Ord e)
    => Array D Ix2 e -> Int -> Map e (Array U Ix2 e)
groupByColumn matrix idx =
    let
        Sz (m :. n) = A.size matrix

        groupingVector :: Array U Ix1 e
        groupingVector = A.compute (matrix <! idx)

        matrix' = A.computeAs U $ dropColumn idx matrix

    -- Alternative solution, that could be faster. Benchmarking would determine
    --     insertRow :: Map e (Array U Ix2 e) -> Int -> Map e (Array U Ix2 e)
    --     insertRow dict rowIdx =
    --         M.insertWith
    --             -- append vertically (stack row on bottom)
    --             (\a b -> A.computeAs U $ A.append' 2 a b)
    --             -- key to insert:
    --             (groupingVector <! rowIdx)
    --             -- row to append:
    --             (A.computeAs U $ A.extractFromTo' (rowIdx :. 0) (rowIdx+1 :. n-1) matrix')
    --             -- our work-in-progress dictionary:
    --             dict
    -- in
    -- F.foldl' insertRow M.empty [0..m-1]
        insertRow :: Map e (Array DL Ix1 e) -> Int -> Map e (Array DL Ix1 e)
        insertRow dict rowIdx =
            M.insertWith
                -- append rows one after another, at the end they will get stacked row on bottom
                (<>)
                -- key to insert:
                (groupingVector ! rowIdx)
                -- row to append:
                (toLoadArray (matrix' !> rowIdx))
                -- our work-in-progress dictionary:
                dict
    in
    unflatten' (n - 1) . A.computeAs U <$> F.foldl' insertRow M.empty [0..m-1]

-- | Take a flat vector and a number of columns and turn it into a matrix.
unflatten' ::
     (Load r Ix1 e, Resize r Ix1) => Ix1 -> Array r Ix1 e -> Array r Ix2 e
unflatten' n arr
  | n <= 0 || isEmpty arr = resize' (Sz2 (unSz (size arr)) 0) arr
  | otherwise =
    case quotRem (unSz (size arr)) n of
      (r, _) -> resize' (Sz2 r n) arr


dropColumn :: Int -> Array D Ix2 e -> Array DL Ix2 e
dropColumn i = either A.throw id . A.deleteColumnsM i 1


groupEntropies :: forall e1 e2 . (Ord e1, Ord e2, Unbox e1, Unbox e2)
    => Array U Ix1 e1 -> Array U Ix1 e2 -> Map e1 (Int, Double)
groupEntropies groupingArray targetArray =
    let
        insertGroup :: Map e1 (Array DL Ix1 e2) -> (e1, e2) -> Map e1 (Array DL Ix1 e2)
        insertGroup dict (e1, e2) =
            M.insertWith (<>) e1 (A.singleton e2) dict
    in
    A.zip groupingArray targetArray
        & F.foldl' insertGroup M.empty
        & fmap A.toLoadArray
        & fmap (\a -> (A.unSz $ A.size a, entropy $ toManifest $ computeAs U a))


groupEntropy :: forall e1 e2 . (Ord e1, Ord e2, Unbox e1, Unbox e2)
    => Array U Ix1 e1 -> Array U Ix1 e2 -> Double
groupEntropy groupingArray targetArray =
    groupEntropies groupingArray targetArray
        & M.elems
        & F.foldl' aggregate 0
    where
        aggregate a (count, entr) = a + entr / fromIntegral count


groupEntropy' :: forall t a1 a2 . (Foldable t)
    => t (a1, a2) -> Double
groupEntropy' things =
    things
        & F.foldl' undefined M.empty
        & M.elems
        & F.foldl' aggregate 0
    where
        aggregate :: Double -> (Int, Double) -> Double
        aggregate a (count, entr) =
            a + entr / fromIntegral count
