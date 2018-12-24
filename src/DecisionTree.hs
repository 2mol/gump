module DecisionTree where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Foreign.Storable (Storable)
--import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric.LinearAlgebra (Element, Matrix, Vector)
import qualified Numeric.LinearAlgebra as N
import Numeric.LinearAlgebra.Data as N

main = do
    csvData <- BL.readFile "data/iris.data"

    -- case decode NoHeader csvData of
        -- Left err -> putStrLn err
        -- Right v -> print v

    pure ()


data DataMatrix = DataMatrix
    { matrix  :: Matrix I
    , classes :: V.Vector String
    } deriving (Show)

