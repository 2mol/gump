{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Impurity
import qualified Data.HashMap.Strict as HMS
import           Data.List
import qualified Data.Text as T
import qualified Data.Vector as V

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
                    step = (hi - lo) / 10.0 in
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
    -> [(Split b, Double)]
gains mkTarget feature datas = do
    split@(Split _ _ f) <- splits feature (V.toList column)
    return (split, gain split)
  where
    column   = V.map (extract feature) datas
    target   = V.map mkTarget datas
    numTotal = fromIntegral (V.length target)

    targetEntropy = Impurity.entropy target

    gain :: Split b -> Double
    gain (Split _ _ f) =
        let (xs, ys)  = V.partition (f . fst) $ V.zip column target
            propXs    = fromIntegral (V.length xs) / numTotal
            propYs    = fromIntegral (V.length ys) / numTotal
            xsEntropy = Impurity.entropy (V.map snd xs)
            ysEntropy = Impurity.entropy (V.map snd ys) in
        targetEntropy - (propXs * xsEntropy + propYs * ysEntropy)

main :: IO ()
main = do
    content <- BL.readFile "data/iris.csv"
    irisses <- either fail return $ Csv.decode Csv.NoHeader content
    putStrLn $ unlines $ map show $ gains
        irisClass
        (float {extract = petalWidth, name = "petalWidth"})
        irisses
