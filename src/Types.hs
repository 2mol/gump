-- yes I know this module is an anti-pattern

module Types where


newtype DataFrame = DataFrame ([Features], Results)


data Features
    = Categorical [Int]
    | Quantitative [Double]


newtype Results = Results [Int]

