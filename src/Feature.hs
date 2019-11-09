{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Feature where

import           Control.Monad.RWS (runRWS)
import           Control.Monad.State (state)
import           Control.Monad.Writer (tell)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HMS
import           Data.List
import           Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V

--------------------------------------------------------------------------------

data Feature s where
    Feature :: String -> (s -> a) -> Strategy a -> Feature s

type Strategy a = V.Vector a -> [Split a]

data Split a = Split String String (a -> Bool)

instance Show (Split a) where
    show (Split x y _) = "Split " ++ show x ++ " " ++ show y ++ " _"

bool :: Strategy Bool
bool = const [Split "True" "False" id]

int :: Strategy Int
int ints =
    [ Split ("< " ++ show pivot) (">= " ++ show pivot) (\x -> x < pivot)
    | pivot <- Set.toAscList $ V.foldl' (flip Set.insert) Set.empty ints
    ]

float :: Strategy Float
float floats | V.null floats = []
float floats =
    let lo   = minimum floats
        hi   = maximum floats
        step = (hi - lo) / 100.0 in
    [ Split ("< " ++ show pivot) (">= " ++ show pivot) (\x -> x < pivot)
    | pivot <- [lo + step, lo + 2 * step .. hi]
    ]

distinct :: (Eq a, Show a) => Strategy a
distinct values =
    [ Split ("== " ++ show val) ("!= " ++ show val) (\x -> x == val)
    | val <- nub $ V.toList values
    ]

--------------------------------------------------------------------------------

data Iris = Iris
    { sepalLength :: !Float
    , sepalWidth  :: !Float
    , petalLength :: !Float
    , petalWidth  :: !Float
    , irisClass   :: !T.Text
    } deriving (Show)

instance Csv.FromRecord Iris where
    parseRecord v = Iris
        <$> v Csv..! 0
        <*> v Csv..! 1
        <*> v Csv..! 2
        <*> v Csv..! 3
        <*> v Csv..! 4

irisFeatures :: [Feature Iris]
irisFeatures =
    [ Feature "sepal length" sepalLength float
    , Feature "sepal width"  sepalWidth  float
    , Feature "petal length" petalLength float
    , Feature "petal width"  petalWidth  float
    ]

data Histogram a = Histogram !(HMS.HashMap a Int) !Int

instance (Eq a, Hashable a) => Semigroup (Histogram a) where
    Histogram m1 t1 <> Histogram m2 t2 =
        Histogram (HMS.unionWith (+) m1 m2) (t1 + t2)

instance (Eq a, Hashable a) => Monoid (Histogram a) where
    mempty = Histogram HMS.empty 0

singleton :: (Hashable a) => a -> Histogram a
singleton x = Histogram (HMS.singleton x 1) 1

entropy :: Histogram a -> Double
entropy (Histogram histo totalCount) = -sum
    [ p * logBase 2 p
    | (_, elemCount) <- HMS.toList histo
    , let p = (fromIntegral elemCount / fromIntegral totalCount)
    ]

total :: Histogram a -> Int
total (Histogram _ t) = t

gains
    :: forall a t. (Eq t, Hashable t) => (a -> t) -> Feature a -> V.Vector a
    -> [(Split a, Double)]
gains target (Feature _ extract strategy) datas = do
    split <- strategy column
    case split of
        Split l r f ->
            let (xs, ys) = V.foldl'
                    (\(!xhisto, !yhisto) (x, t) ->
                        if f x
                            then (xhisto <> singleton t, yhisto)
                            else (xhisto, yhisto <> singleton t))
                    mempty
                    (V.zip column targets)

                px   = fromIntegral (total xs) / numTotal
                py   = fromIntegral (total ys) / numTotal
                ex   = entropy xs
                ey   = entropy ys
                gain = (targetEntropy - (px * ex + py * ey)) in
            return (Split l r (f . extract), gain)
  where
    column   = V.map extract datas
    targets  = V.map target  datas
    numTotal = fromIntegral (V.length targets)

    targetEntropy = entropy $ foldMap singleton targets

data Tree a t
    = Leaf   t Double
    | Branch String (Split a) (Tree a t) (Tree a t)

treeToGraph
    :: (t -> String) -> Tree a t -> [String]
treeToGraph mkLeafLabel = \tree ->
    let (_, _, strs) = runRWS (toGraph tree) () (0 :: Int) in strs
  where
    toGraph tree = do
        tell ["digraph graphname {"]
        _ <- go tree
        tell ["}"]

    go (Leaf x d) = do
        name <- fresh
        let label = mkLeafLabel x ++ " (" ++ show d ++ ")"
        tell [name ++ " [label=" ++ show label ++ "];"]
        return name

    go (Branch attr (Split ledge redge _) ltree rtree) = do
        name <- fresh
        tell [name ++ " [label=" ++ show attr ++ "];"]
        lname <- go ltree
        rname <- go rtree
        tell [name ++ " -> " ++ lname ++ " [label=" ++ show ledge ++ "];"]
        tell [name ++ " -> " ++ rname ++ " [label=" ++ show redge ++ "];"]
        return name

    fresh = state $ \counter -> ("node" ++ show counter, counter + 1)

-- printTree :: Show t => Tree a t -> [String]
-- printTree (Leaf x d) = [show x ++ " (" ++ show d ++ ")"]
-- printTree (Branch attr (Split ledge redge _) ltree rtree) =
--     [attr] ++
--     [" " ++ ledge] ++
--     map ("  " ++) (printTree ltree) ++
--     [" " ++ redge] ++
--     map ("  " ++) (printTree rtree)

makeLeaf :: (Eq t, Hashable t) => (a -> t) -> V.Vector a -> Tree a t
makeLeaf target datas =
    let histo@(Histogram freqs _) = foldMap (singleton . target) datas
        (t, count) = maximumBy (comparing snd) (HMS.toList freqs) in
    Leaf t (fromIntegral count / fromIntegral (total histo))

makeTree
    :: forall a t. (Eq t, Hashable t)
    => Int          -- ^ Max depth
    -> (a -> t)     -- ^ Target
    -> [Feature a]  -- ^ Features
    -> V.Vector a   -- ^ Data
    -> Tree a t
makeTree maxDepth target features = go 0
  where
    go depth datas | depth >= maxDepth =
        makeLeaf target datas

    go _depth datas | [single] <- nub (map target $ V.toList datas) =
        Leaf single 1.0

    go depth datas =
        let allGains :: [(String, (Split a, Double))]
            allGains =
                [ (name, gain)
                | feature@(Feature name _ _) <- features
                , gain                       <- gains target feature datas
                ]

            (fname, (split@(Split _ _ f), _)) =
                maximumBy (comparing (\(_, (_, g)) -> g))
                allGains

            (xs, ys) = V.partition f datas in

        Branch fname split
            (go (depth + 1) xs)
            (go (depth + 1) ys)

main :: IO ()
main = do
    content <- BL.readFile "data/iris.csv"
    irisses <- either fail return $ Csv.decode Csv.NoHeader content
    writeFile "iris.dot" $ unlines $ treeToGraph T.unpack $ makeTree
        3
        irisClass
        irisFeatures
        irisses
