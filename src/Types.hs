-- yes I know this module is an anti-pattern

module Types where


newtype DataFrame = DataFrame ([Feature], Target)


data Feature
    = Categorical [Int]
    | Continuous [Double]


newtype Target = Target [Int]
