{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DecisionTree where

import qualified Data.Foldable as F
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Prelude as P

import Data.Massiv.Array (Array, D, Ix1, Ix2(..), U(..), Unbox, (!>), (<!))
import qualified Data.Massiv.Array as A


irisData :: Array U Ix2 Double
irisData = A.fromLists' A.Par
    [ [ 5.1, 3.5, 1.4, 0.2 ]
    , [ 4.9, 3.0, 1.4, 0.2 ]
    , [ 4.7, 3.2, 1.3, 0.2 ]
    , [ 4.6, 3.1, 1.5, 0.2 ]
    , [ 5.0, 3.6, 1.4, 0.2 ]
    , [ 5.4, 3.9, 1.7, 0.4 ]
    , [ 4.6, 3.4, 1.4, 0.3 ]
    , [ 5.0, 3.4, 1.5, 0.2 ]
    , [ 4.4, 2.9, 1.4, 0.2 ]
    , [ 4.9, 3.1, 1.5, 0.1 ]
    , [ 5.4, 3.7, 1.5, 0.2 ]
    , [ 4.8, 3.4, 1.6, 0.2 ]
    , [ 4.8, 3.0, 1.4, 0.1 ]
    , [ 4.3, 3.0, 1.1, 0.1 ]
    , [ 5.8, 4.0, 1.2, 0.2 ]
    , [ 5.7, 4.4, 1.5, 0.4 ]
    , [ 5.4, 3.9, 1.3, 0.4 ]
    , [ 5.1, 3.5, 1.4, 0.3 ]
    , [ 5.7, 3.8, 1.7, 0.3 ]
    , [ 5.1, 3.8, 1.5, 0.3 ]
    , [ 5.4, 3.4, 1.7, 0.2 ]
    , [ 5.1, 3.7, 1.5, 0.4 ]
    , [ 4.6, 3.6, 1.0, 0.2 ]
    , [ 5.1, 3.3, 1.7, 0.5 ]
    , [ 4.8, 3.4, 1.9, 0.2 ]
    , [ 5.0, 3.0, 1.6, 0.2 ]
    , [ 5.0, 3.4, 1.6, 0.4 ]
    , [ 5.2, 3.5, 1.5, 0.2 ]
    , [ 5.2, 3.4, 1.4, 0.2 ]
    , [ 4.7, 3.2, 1.6, 0.2 ]
    , [ 4.8, 3.1, 1.6, 0.2 ]
    , [ 5.4, 3.4, 1.5, 0.4 ]
    , [ 5.2, 4.1, 1.5, 0.1 ]
    , [ 5.5, 4.2, 1.4, 0.2 ]
    , [ 4.9, 3.1, 1.5, 0.1 ]
    , [ 5.0, 3.2, 1.2, 0.2 ]
    , [ 5.5, 3.5, 1.3, 0.2 ]
    , [ 4.9, 3.1, 1.5, 0.1 ]
    , [ 4.4, 3.0, 1.3, 0.2 ]
    , [ 5.1, 3.4, 1.5, 0.2 ]
    , [ 5.0, 3.5, 1.3, 0.3 ]
    , [ 4.5, 2.3, 1.3, 0.3 ]
    , [ 4.4, 3.2, 1.3, 0.2 ]
    , [ 5.0, 3.5, 1.6, 0.6 ]
    , [ 5.1, 3.8, 1.9, 0.4 ]
    , [ 4.8, 3.0, 1.4, 0.3 ]
    , [ 5.1, 3.8, 1.6, 0.2 ]
    , [ 4.6, 3.2, 1.4, 0.2 ]
    , [ 5.3, 3.7, 1.5, 0.2 ]
    , [ 5.0, 3.3, 1.4, 0.2 ]
    , [ 7.0, 3.2, 4.7, 1.4 ]
    , [ 6.4, 3.2, 4.5, 1.5 ]
    , [ 6.9, 3.1, 4.9, 1.5 ]
    , [ 5.5, 2.3, 4.0, 1.3 ]
    , [ 6.5, 2.8, 4.6, 1.5 ]
    , [ 5.7, 2.8, 4.5, 1.3 ]
    , [ 6.3, 3.3, 4.7, 1.6 ]
    , [ 4.9, 2.4, 3.3, 1.0 ]
    , [ 6.6, 2.9, 4.6, 1.3 ]
    , [ 5.2, 2.7, 3.9, 1.4 ]
    , [ 5.0, 2.0, 3.5, 1.0 ]
    , [ 5.9, 3.0, 4.2, 1.5 ]
    , [ 6.0, 2.2, 4.0, 1.0 ]
    , [ 6.1, 2.9, 4.7, 1.4 ]
    , [ 5.6, 2.9, 3.6, 1.3 ]
    , [ 6.7, 3.1, 4.4, 1.4 ]
    , [ 5.6, 3.0, 4.5, 1.5 ]
    , [ 5.8, 2.7, 4.1, 1.0 ]
    , [ 6.2, 2.2, 4.5, 1.5 ]
    , [ 5.6, 2.5, 3.9, 1.1 ]
    , [ 5.9, 3.2, 4.8, 1.8 ]
    , [ 6.1, 2.8, 4.0, 1.3 ]
    , [ 6.3, 2.5, 4.9, 1.5 ]
    , [ 6.1, 2.8, 4.7, 1.2 ]
    , [ 6.4, 2.9, 4.3, 1.3 ]
    , [ 6.6, 3.0, 4.4, 1.4 ]
    , [ 6.8, 2.8, 4.8, 1.4 ]
    , [ 6.7, 3.0, 5.0, 1.7 ]
    , [ 6.0, 2.9, 4.5, 1.5 ]
    , [ 5.7, 2.6, 3.5, 1.0 ]
    , [ 5.5, 2.4, 3.8, 1.1 ]
    , [ 5.5, 2.4, 3.7, 1.0 ]
    , [ 5.8, 2.7, 3.9, 1.2 ]
    , [ 6.0, 2.7, 5.1, 1.6 ]
    , [ 5.4, 3.0, 4.5, 1.5 ]
    , [ 6.0, 3.4, 4.5, 1.6 ]
    , [ 6.7, 3.1, 4.7, 1.5 ]
    , [ 6.3, 2.3, 4.4, 1.3 ]
    , [ 5.6, 3.0, 4.1, 1.3 ]
    , [ 5.5, 2.5, 4.0, 1.3 ]
    , [ 5.5, 2.6, 4.4, 1.2 ]
    , [ 6.1, 3.0, 4.6, 1.4 ]
    , [ 5.8, 2.6, 4.0, 1.2 ]
    , [ 5.0, 2.3, 3.3, 1.0 ]
    , [ 5.6, 2.7, 4.2, 1.3 ]
    , [ 5.7, 3.0, 4.2, 1.2 ]
    , [ 5.7, 2.9, 4.2, 1.3 ]
    , [ 6.2, 2.9, 4.3, 1.3 ]
    , [ 5.1, 2.5, 3.0, 1.1 ]
    , [ 5.7, 2.8, 4.1, 1.3 ]
    , [ 6.3, 3.3, 6.0, 2.5 ]
    , [ 5.8, 2.7, 5.1, 1.9 ]
    , [ 7.1, 3.0, 5.9, 2.1 ]
    , [ 6.3, 2.9, 5.6, 1.8 ]
    , [ 6.5, 3.0, 5.8, 2.2 ]
    , [ 7.6, 3.0, 6.6, 2.1 ]
    , [ 4.9, 2.5, 4.5, 1.7 ]
    , [ 7.3, 2.9, 6.3, 1.8 ]
    , [ 6.7, 2.5, 5.8, 1.8 ]
    , [ 7.2, 3.6, 6.1, 2.5 ]
    , [ 6.5, 3.2, 5.1, 2.0 ]
    , [ 6.4, 2.7, 5.3, 1.9 ]
    , [ 6.8, 3.0, 5.5, 2.1 ]
    , [ 5.7, 2.5, 5.0, 2.0 ]
    , [ 5.8, 2.8, 5.1, 2.4 ]
    , [ 6.4, 3.2, 5.3, 2.3 ]
    , [ 6.5, 3.0, 5.5, 1.8 ]
    , [ 7.7, 3.8, 6.7, 2.2 ]
    , [ 7.7, 2.6, 6.9, 2.3 ]
    , [ 6.0, 2.2, 5.0, 1.5 ]
    , [ 6.9, 3.2, 5.7, 2.3 ]
    , [ 5.6, 2.8, 4.9, 2.0 ]
    , [ 7.7, 2.8, 6.7, 2.0 ]
    , [ 6.3, 2.7, 4.9, 1.8 ]
    , [ 6.7, 3.3, 5.7, 2.1 ]
    , [ 7.2, 3.2, 6.0, 1.8 ]
    , [ 6.2, 2.8, 4.8, 1.8 ]
    , [ 6.1, 3.0, 4.9, 1.8 ]
    , [ 6.4, 2.8, 5.6, 2.1 ]
    , [ 7.2, 3.0, 5.8, 1.6 ]
    , [ 7.4, 2.8, 6.1, 1.9 ]
    , [ 7.9, 3.8, 6.4, 2.0 ]
    , [ 6.4, 2.8, 5.6, 2.2 ]
    , [ 6.3, 2.8, 5.1, 1.5 ]
    , [ 6.1, 2.6, 5.6, 1.4 ]
    , [ 7.7, 3.0, 6.1, 2.3 ]
    , [ 6.3, 3.4, 5.6, 2.4 ]
    , [ 6.4, 3.1, 5.5, 1.8 ]
    , [ 6.0, 3.0, 4.8, 1.8 ]
    , [ 6.9, 3.1, 5.4, 2.1 ]
    , [ 6.7, 3.1, 5.6, 2.4 ]
    , [ 6.9, 3.1, 5.1, 2.3 ]
    , [ 5.8, 2.7, 5.1, 1.9 ]
    , [ 6.8, 3.2, 5.9, 2.3 ]
    , [ 6.7, 3.3, 5.7, 2.5 ]
    , [ 6.7, 3.0, 5.2, 2.3 ]
    , [ 6.3, 2.5, 5.0, 1.9 ]
    , [ 6.5, 3.0, 5.2, 2.0 ]
    , [ 6.2, 3.4, 5.4, 2.3 ]
    , [ 5.9, 3.0, 5.1, 1.8 ]
    ]

data IrisSpecies = Setosa | Versicolor | Virginica

irisDataSpecies :: [IrisSpecies]
irisDataSpecies =
    [ Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Setosa
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Versicolor
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    , Virginica
    ]
{-
how to build a decision tree:

0. put data matrix values in buckets, if necessary. Keep dict of that mapping.
1. get information gains for each column.
2. see which column has the highest gain.
3. perform `groupBy` on matrix (and feature vector) by that colum.
4. recursively repeat with those smaller matrices in each group.

-}

-- 1. a) entropy measure

countElements :: (Foldable t, Ord a) => t a -> Map a Int
countElements xs =
    F.foldl' insertCount M.empty xs
    where
        insertCount :: Ord a => Map a Int -> a -> Map a Int
        insertCount counter el = M.insertWith (+) el 1 counter

entropy :: (Foldable t, Ord a) => t a -> Double
entropy xs =
    let
        counts = M.elems (countElements xs)

        len = fromIntegral $ length xs

        proportions =
            [ c / len | c' <- counts, let c = fromIntegral c' ]

        componentEntropies =
            [ -p * logBase 2 p | p <- proportions ]
    in
    F.sum componentEntropies

-- generic groupBy

groupByColumn :: forall e . (Unbox e, Ord e) => Array D Ix2 e -> Int -> Map e (Array D Ix2 e)
groupByColumn matrix idx =
    let
        (m :. n) = A.size matrix

        groupingVector = A.computeAs U (matrix <! idx)

        matrix' = dropColumn idx matrix

        insertRow :: Map e (Array D Ix2 e) -> Int -> Map e (Array D Ix2 e)
        insertRow dict rowIdx =
            M.insertWith
                (A.append' 2)
                (groupingVector <! rowIdx)
                (A.extractFromTo' (rowIdx :. 0) (rowIdx+1 :. n-1) matrix')
                dict
    in
    F.foldl' insertRow M.empty [0..m-1]
        -- & M.map A.compute

dropColumn :: (Unbox e) => Int -> Array D Ix2 e -> Array D Ix2 e
dropColumn idx matrix =
    let (m :. n) = A.size matrix
        left  = A.extractFromTo' (0 :. 0) (m :. idx) matrix
        right = A.extractFromTo' (0 :. idx+1) (m :. n) matrix
    in
    A.append' 1 left right

--

main :: IO ()
main = do
    pure ()

