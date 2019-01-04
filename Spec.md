# Specifications

## Decision Trees

1. calculate the entropy `H(S)` of classification (set `S`).
2. calculate the average entropies `sum H(S_t)` for each feature/category `t` in `T`.
3. information gain = `H(S) - sum H(S_t)`.
    - observation: since we look for the max gain, we can just look at min avg entropy, because `H(S)` will stay the same.

## Example

1. sepal length
2. sepal width
3. petal length
4. petal width
5. class

5,1 3,5 1,4 0,2 Iris-setosa
4,9 3   1,4 0,2 Iris-setosa
4,7 3,2 1,3 0,2 Iris-setosa
4,6 3,1 1,5 0,2 Iris-setosa
5   3,6 1,4 0,2 Iris-setosa

### Want

- a matrix where we keep track of all the indices, so that we can do recursive groupbys
- elements should be in buckets, so we should convert Double to Int buckets
- we keep several maps around:
    - column labels, and their original indices
    - row labels (or just index numbers)
    - data buckets, so something like `Map Int [a]` where a is our original data type (generally `Double`). Or better: `Map a Int`
- last column is the classes! So we want a classification that we keep track of. Will be `String`, naively.

## How

Problems:

- how the FUCK do we just read in a csv and have something like a dataframe in haskell?
    - cassava: couldn't figure out how to make number of columns dynamic
    - frames?
    - labels?
    - hmatrix?
    - massiv?

## Resources

- data: https://archive.ics.uci.edu/ml/index.php
- `Frames` tutorial:
    - https://idontgetoutmuch.wordpress.com/2018/05/19/cartography-in-haskell/
    - https://github.com/idontgetoutmuch/psephology

## Language choice

### Python

pros:

- getting started quickly
- having pandas
- easy visualization (probably)

cons:

- no types

### Haskell

pros:

- learning effect
- types, conceptually clearer how data is transformed

cons:

- no dataframes

