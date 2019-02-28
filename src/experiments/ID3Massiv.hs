{-# LANGUAGE OverloadedStrings #-}
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
import Data.Function ((&))
-- import Data.Int (Int8)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Massiv.Array (Array, D, Ix1, Ix2(..), M, U(..), Unbox, (<!))
import qualified Data.Massiv.Array as A
-- import Data.Maybe (catMaybes)
-- import Data.Text (Text)

-- import TestData
import CommonMath (entropy)

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
    => Array D Ix2 e -> Int -> Map e (Array D Ix2 e)
groupByColumn matrix idx =
    let
        (m :. n) = A.size matrix

        groupingVector :: Array U Ix1 e
        groupingVector = A.compute (matrix <! idx)

        matrix' = dropColumn idx matrix

        insertRow :: Map e (Array D Ix2 e) -> Int -> Map e (Array D Ix2 e)
        insertRow dict rowIdx =
            M.insertWith
                -- append vertically (stack row on bottom)
                (A.append' 2)
                -- key to insert:
                (groupingVector <! rowIdx)
                -- row to append:
                (A.extractFromTo' (rowIdx :. 0) (rowIdx+1 :. n-1) matrix')
                -- our work-in-progress dictionary:
                dict
    in
    F.foldl' insertRow M.empty [0..m-1]

dropColumn :: Int -> Array D Ix2 e -> Array D Ix2 e
dropColumn i matrix =
    let (m :. n) = A.size matrix
        left  = A.extractFromTo' (0 :. 0)   (m :. i) matrix
        right = A.extractFromTo' (0 :. i+1) (m :. n) matrix
    in
    A.append' 1 left right

groupEntropies :: forall e1 e2 . (Ord e1, Ord e2)
    => Array M Ix1 e1 -> Array M Ix1 e2 -> Map e1 (Int, Double)
groupEntropies groupingArray targetArray =
    let
        insertGroup :: Map e1 (Array D Ix1 e2) -> (e1, e2) -> Map e1 (Array D Ix1 e2)
        insertGroup dict (e1, e2) =
            M.insertWith (A.append' 1) e1 (A.singleton A.Par e2) dict
    in
        A.zip groupingArray targetArray
            & F.foldl' insertGroup M.empty
            & fmap (\a -> (A.size a, entropy a))

groupEntropy :: forall e1 e2 . (Ord e1, Ord e2)
    => Array M Ix1 e1 -> Array M Ix1 e2 -> Double
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
        -- & fmap (\a -> (F.length a, entropy a))
        & M.elems
        & F.foldl' aggregate 0
    where
        aggregate :: Double -> (Int, Double) -> Double
        aggregate a (count, entr) =
            a + entr / fromIntegral count

--

--categorize :: (Foldable t, Ord a) => t a -> Map Int8 a
-- categorize things =
    -- F.foldl' (\dict k -> M.insert k () dict) M.empty things
        -- & fmap undefined

main :: IO ()
main = do
    putStrLn "fafa"
    pure ()

