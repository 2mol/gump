{-# LANGUAGE ScopedTypeVariables #-}

module DecisionTree where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Numeric.LinearAlgebra

main = do
    csvData <- BL.readFile "data/iris.data"

    -- case decode NoHeader csvData of
        -- Left err -> putStrLn err
        -- Right v -> print v

    pure ()


-- want: Matrix a -> Map k (Matrix a)

data DataMatrix r c = DataMatrix
    { matrix :: Matrix Double
    , rows   :: V.Vector r
    , cols   :: V.Vector c
    } deriving (Show)

