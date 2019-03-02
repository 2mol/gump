module ID3 where

{-
Description from [https://scikit-learn.org/stable/modules/tree.html]:

ID3 (Iterative Dichotomiser 3) was developed in 1986 by Ross Quinlan. The
algorithm creates a multiway tree, finding for each node (i.e. in a greedy
manner) the categorical feature that will yield the largest information gain
for categorical targets. Trees are grown to their maximum size and then a
pruning step is usually applied to improve the ability of the tree to
generalise to unseen data.
-}

-- import qualified Data.Foldable as F
import           Data.Function ((&))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Tuple (swap)
import           Data.Word (Word8)

-- import qualified Impurity

{-
how to build a decision tree with categorical data:

TODO
-}


newtype DataFrame =
    DataFrame ([VecFeature], VecTarget)

newtype VecFeature = VecFeature [Word8]

newtype VecTarget = VecTarget [Word8]


-- makeDataFrame' :: (Ord a, Ord b) => [[a]] -> [b] -> DataFrame
makeDataFrame' :: [[a]] -> [b] -> DataFrame
makeDataFrame' =
    undefined

-- | Takes a list of values of any orderable type and assigns an 8-bit unsigned
-- integer to them.
categorize :: Ord a => [a] -> Map Word8 a
categorize xs =
    zip xs (repeat ())
        & M.fromList
        & assignCategories
        & reverseMap
    where
        assignCategories :: Map a () -> Map a Word8
        assignCategories dict =
            M.mapAccum (\acc () -> (acc+1, acc)) 0 dict
                & snd

        agg :: Word8 -> () -> (Word8, Word8)
        agg

-- | Reverses a 'Map' from key -> val to val -> key.
-- Duplicate values and their respective keys will be lost,
-- so this function is meant to be used when the values themselves are unique.
reverseMap :: Ord v => Map k v -> Map v k
reverseMap = M.fromList . map swap . M.toList


-- uniques :: [a] -> Map a ()
-- uniques = undefined

-- bla :: Map a () -> Map a Int
-- bla = undefined



