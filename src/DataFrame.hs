module DataFrame where

import           Data.Function ((&))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Tuple (swap)


newtype DataFrame = DataFrame ([Feature], Target)


data Feature
    = Categorical [Int]
    | Continuous [Double]


newtype Target = Target [Int]

-- helper functions

categorize :: [a] -> Map Int a
categorize = undefined

-- uniques :: [a] -> Map a ()
-- uniques = undefined

-- bla :: Map a () -> Map a Int
-- bla = undefined

reverseMap :: Ord v => Map k v -> Map v k
reverseMap = M.fromList . map swap . M.toList


makeDataFrame' :: [[Double]] -> [a] -> DataFrame
makeDataFrame' featuresData targetData =
    DataFrame (featureVectors, targetVector)
    where
        featureVectors = map Continuous featuresData
        targetVector =
            categorize targetData
                & M.keys
                & Target


makeDataFrame :: [[a]] -> [b] -> DataFrame
makeDataFrame = undefined

