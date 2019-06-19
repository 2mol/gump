{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

main :: IO ()
main = do
    let groupedByColumn1 = groupBy exampleTable 1
    if groupedByColumn1 == expectedGroupByColumn1
        then putStrLn "\\( ﾟヮﾟ)/  yay"
        else putStrLn "(✖╭╮✖)  oh no"
    print groupedByColumn1

groupBy :: forall v . Ord v => [[v]] -> Int -> Map v [[v]]
groupBy table columnIndex =
    foldr insertRow M.empty splitTable
    where
        insertRow :: (v, [v]) -> Map v [[v]] -> Map v [[v]]
        insertRow (k, vs) dict =
            -- M.insertWith :: ([[v]] -> [[v]] -> [[v]]) -> v -> [[v]] -> Map v [[v]] -> Map v [[v]]
            M.insertWith (++) k [vs] dict
            -- puzzle: why is M.insertWith not of type (b -> a -> a) -> k -> b -> Map k a -> Map k a
            -- it would allow for the more efficient `M.insertWith (:) k vs dict`

        splitTable :: [(v, [v])]
        splitTable =
            map (split columnIndex) table

split :: Int -> [v] -> (v, [v])
split i row =
    (row !! i, take i row ++ drop (i+1) row)

exampleTable :: [[Int]]
exampleTable =
    [ [1,1,6,3]
    , [2,1,7,1]
    , [3,2,8,4]
    , [4,2,9,1]
    , [5,2,0,5]
    ]

expectedGroupByColumn1 :: Map Int [[Int]]
expectedGroupByColumn1 = M.fromList
    [ (1,
        [ [1,6,3]
        , [2,7,1]
        ])
    , (2,
        [ [3,8,4]
        , [4,9,1]
        , [5,0,5]
        ])
    ]
