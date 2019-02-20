-- yes I know this module is an anti-pattern

module Types where


newtype DataFrame = DataFrame ([Features], Results)


data Features
    = Categorical [Int]
    | Continuous [Double]


newtype Results = Results [Int]
