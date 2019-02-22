# Gump

an attempt to learn decision trees and random forests by implementing them myself.

## explanation

basically, decision trees try to answer the question "which feature makes the biggest difference"

## approach

my understanding of the algorithm(s). numbers like 1. are a necessary step, while bulletpoints are different choices we could make.


1. read dataframe into a list of feature vectors and one target vector.
2. look at each feature and measure information gain **if** we'd split on that one.
  - variable can be continuous or categorical, changing the way we split.
    - on categorical variables we can split on every value **or** do a binary split.
    - on continuous variables we can find an optimal binary split value **or** we can discretize values, meaning we kinda get categorical buckets.
3. find the feature with the highest value split, and perform that split.
4. for each branch of that split (2 if binary, n if fully categorical) we get a filtered down subset of the measurement data above.
5. repeat recursively.

Note: make sure to have a stopping condition, so that splits don't result in too few datapoints.

## open questions

- does the target always have to be categorical?
- bagging seems to be a bit of a choice, i.e. how fine-grained do we want to make the buckets
  - can this choice be optimized as well?
  - seems non-trivial, because a greedy algorithm might way overfit at this step

