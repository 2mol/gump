module DataFrame where


data DDataFrame a = DDataFrame
    { features   :: Map String [Word8]
    , target     :: [Word8]
    , featureCat :: Map String (Map Word8 a)
    , targetCat  :: Map Word8 a
    }
    deriving Show
