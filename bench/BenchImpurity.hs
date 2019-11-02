{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad (unless)
import Data.Massiv.Array as A
import Criterion.Main
import System.Random
import Impurity as L
import ImpurityMassiv as A
import Data.List as List (sort)

main :: IO ()
main = do
  let massiv = A.toList $ computeAs U $ A.tally $ computeAs P $ makeRandomArray 20
      mapList = L.tallyM $ A.toList $ computeAs P $ makeRandomArray 20
  -- Sanity check:
  unless (List.sort massiv == List.sort mapList) $ error "Results do not match"
  defaultMain
    [ mkImpurityBenchmarks 20
    , mkImpurityBenchmarks 2000
    , mkImpurityBenchmarks 200000
    ]

makeRandomArray :: Int -> Array DL Ix1 Int
makeRandomArray maxVal =
  let n = Sz1 160000
      gen = mkStdGen 2019
   in randomArray gen split (randomR (0, maxVal)) Par n



mkImpurityBenchmarks :: Int -> Benchmark
mkImpurityBenchmarks k =
  bgroup
    ("Impurity " ++ show k)
    [ env (return (computeAs P $ makeRandomArray k)) $ \arr ->
        bgroup
          "Massiv"
          [bench "Tally" $ nf (computeAs U . A.tally) arr]
    , env (return (A.toList $ computeAs P $ makeRandomArray k)) $ \xs ->
        bgroup "List+Map" [bench "Tally" $ nf L.tally xs]
    ]
