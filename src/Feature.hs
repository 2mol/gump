{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Feature where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
-- import qualified Data.Graph.Inductive.Graph as Graph
-- import qualified Data.HashMap.Strict as HMS
import           Data.List
import Control.Monad.RWS (runRWS)
import Control.Monad.Writer (tell)
import Control.Monad.State (state)
import qualified Data.Map as Map
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Impurity

--------------------------------------------------------------------------------

-- | s: [Double]
--   a: Double
--
--   s: Survivor = Survivor {age :: Int, ... sex :: Sex }
--   a: Sex
data Feature s a = Feature
    { name    :: String
    , extract :: s -> a
    , splits  :: [a] -> [Split a]
    }

data Split a = Split String String (a -> Bool)

instance Show (Split a) where
    show (Split x y _) = "Split " ++ show x ++ " " ++ show y ++ " _"

bool :: Feature Bool Bool
bool = Feature
    { name    = "bool"
    , extract = id
    , splits  = \_ -> [Split "True" "False" id]
    }

int :: Feature Int Int
int = Feature
    { name    = "int"
    , extract = id
    , splits  = \ints ->
        [ Split ("< " ++ show pivot) (">= " ++ show pivot) (\x -> x < pivot)
        | pivot <- drop 1 $ nub $ sort ints
        ]
    }

float :: Feature Float Float
float = Feature
    { name    = "float"
    , extract = id
    , splits  = \floats ->
        if null floats
            then []
            else
                let lo   = minimum floats
                    hi   = maximum floats
                    step = (hi - lo) / 100.0 in
                [ Split ("< " ++ show pivot) (">= " ++ show pivot) (\x -> x < pivot)
                | pivot <- [lo + step, lo + 2 * step .. hi]
                ]
    }

distinct :: (Eq a, Show a) => Feature a a
distinct = Feature
    { name    = "distinct"
    , extract = id
    , splits  = \values ->
        [ Split ("== " ++ show val) ("!= " ++ show val) (\x -> x == val)
        | val <- values
        ]
    }



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

irisFeatures :: [Feature Iris Float]
irisFeatures =
    [ float {extract = sepalLength, name = "sepalLength"}
    , float {extract = sepalWidth, name = "sepalWidth"}
    , float {extract = petalLength, name = "petalLength"}
    , float {extract = petalWidth, name = "petalWidth"}
    ]

gains
    :: forall a b t. Ord t => (a -> t) -> Feature a b -> V.Vector a
    -> [(Split b, (Double, V.Vector a, V.Vector a))]
gains mkTarget feature datas = do
    split <- splits feature (map (extract feature) $ V.toList datas)
    return (split, gain split)
  where
    target   = V.map mkTarget datas
    numTotal = fromIntegral (V.length target)

    targetEntropy = Impurity.entropy target

    gain :: Split b -> (Double, V.Vector a, V.Vector a)
    gain (Split _ _ f) =
        let (xs, ys)  = V.partition (f . extract feature . fst) $ V.zip datas target
            propXs    = fromIntegral (V.length xs) / numTotal
            propYs    = fromIntegral (V.length ys) / numTotal
            xsEntropy = Impurity.entropy (V.map snd xs)
            ysEntropy = Impurity.entropy (V.map snd ys) in
        (targetEntropy - (propXs * xsEntropy + propYs * ysEntropy), V.map fst xs, V.map fst ys)

data Tree a b t
    = Leaf t Double
    | Branch String (Split b) (Tree a b t) (Tree a b t)
    deriving (Show)

treeToGraph
    :: (t -> String) -> Tree a b t -> [String]
treeToGraph mkLeafLabel tree =
    let (_, _, strs) = runRWS (toGraph tree) () 0 in strs
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


printTree :: Show t => Tree a b t -> [String]
printTree (Leaf x d) = [show x ++ " (" ++ show d ++ ")"]
printTree (Branch attr (Split ledge redge _) ltree rtree) =
    [attr] ++
    [" " ++ ledge] ++
    map ("  " ++) (printTree ltree) ++
    [" " ++ redge] ++
    map ("  " ++) (printTree rtree)

makeLeaf :: Ord t => (a -> t) -> V.Vector a -> Tree a b t
makeLeaf mkTarget datas =
    let tally           = Impurity.tally (fmap mkTarget datas)
        total           = fromIntegral $ V.length datas
        (target, count) = maximumBy (comparing snd) (Map.toList tally) in
    Leaf target (fromIntegral count / total)

makeTree
    :: forall a b t. Ord t
    => Int            -- ^ Max depth
    -> (a -> t)       -- ^ Target
    -> [Feature a b]  -- ^ Features
    -> V.Vector a     -- ^ Data
    -> Tree a b t
makeTree maxDepth mkTarget features = go 0
  where
    go depth datas | depth >= maxDepth =
        makeLeaf mkTarget datas

    go depth datas | [single] <- nub (map mkTarget $ V.toList datas) =
        Leaf single 1.0

    go depth datas =
        let allGains :: [(String, (Split b, (Double, V.Vector a, V.Vector a)))]
            allGains =
                [ (name feature, gain)
                | feature <- features
                , gain    <- gains mkTarget feature datas
                ]

            (fname, (split, (_, xs, ys))) =
                maximumBy (comparing (\(_, (_, (g, _, _))) -> g))
                allGains in

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
