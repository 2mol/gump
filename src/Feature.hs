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
import qualified Numeric

--------------------------------------------------------------------------------

data Feature s where
    Feature :: String -> (s -> a) -> Strategy a -> Feature s

type Strategy a =
    -- a strategy should give a list of split candidates based on the vector
    -- of values that we observe. Afterwards we'll try to find the "best" split.
    V.Vector a -> [Split a]

data Split a = Split
    { leftLabel  :: String     -- the label for the left branch of the split
    , rightLabel :: String     -- the label for the right branch of the split
    , splitter  :: (a -> Bool) -- a function that decides which value goes into which branch
    }

instance Show (Split a) where
    show (Split x y _) = "Split " ++ show x ++ " " ++ show y ++ " _"


--------------------------------------------------------------------------------
-- Standard strategies

showFloat :: RealFloat a => a -> String
showFloat n = Numeric.showEFloat (Just 2) n ""

-- There is only one sensible way of splitting a list of booleans,
-- so we choose to put True into the left branch and False into the right one.
bool :: Strategy Bool
bool _ = [Split "True" "False" id]


-- One integer strategy is to just split on each one of the possible values.
-- This will be inefficient if the number of possible values is very large.
int :: Strategy Int
int values | V.null values = []
int values =
    [ Split ("< " ++ show pivot) (">= " ++ show pivot) (\x -> x < pivot)
    | let uniques = V.foldl' (\set el -> Set.insert el set) Set.empty values
    , pivot <- Set.toAscList uniques
    ]


-- One naïve float strategy is to split in the middle between each of the possible values.
-- Like the similar int strategy, this is not suited for a very large number
-- of distinct values.
float :: Strategy Float
float values | V.null values = []
float values =
    [ Split ("< " ++ showFloat pivot) (">= " ++ showFloat pivot) (\x -> x < pivot)
    | let uniques = V.foldl' (\set el -> Set.insert el set) Set.empty values
    , let sUniques = (Set.toAscList uniques)
    , let middles = zipWith (\n1 n2 -> (n1 + n2) / 2) sUniques (tail sUniques)
    , pivot <- middles
    ]


-- another strategy for floats is to try a given number of splits between the
-- min and max of all possible values.
floatStep :: Int -> Strategy Float
floatStep _ floats | V.null floats = []
floatStep nsteps floats =
    let lo   = minimum floats
        hi   = maximum floats
        step = (hi - lo) / (fromIntegral nsteps) in
    [ Split ("< " ++ showFloat pivot) (">= " ++ showFloat pivot) (\x -> x < pivot)
    | pivot <- [lo + step, lo + 2 * step .. hi]
    ]


-- We can generalize the strategy with a given number of steps to other
-- numerical types.
numericalStep :: (RealFloat i, Enum i) => Int -> Strategy i
numericalStep _ floats | V.null floats = []
numericalStep nsteps floats =
    let lo   = minimum floats
        hi   = maximum floats
        step = (hi - lo) / (fromIntegral nsteps) in
    [ Split ("< " ++ showFloat pivot) (">= " ++ showFloat pivot) (\x -> x < pivot)
    | pivot <- [lo + step, lo + 2 * step .. hi]
    ]


-- We can generate a strategy simply on the basis of provided splitting points.
steps :: (Ord i, Show i) => [i] -> Strategy i
steps pivots _ =
    [ Split ("< " ++ show pivot) (">= " ++ show pivot) (\x -> x < pivot)
    | pivot <- pivots
    ]


-- A strategy for categorical values is to split between one particular
-- value and everything else.
categorical :: (Eq a, Show a) => Strategy a
categorical values =
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

data Histogram a = Histogram
    { tally :: !(HMS.HashMap a Int)
    , total :: !Int
    }

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
