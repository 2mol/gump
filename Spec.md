# Specifications

## Decision Trees

1. calculate the entropy `H(S)` of classification (set `S`).
2. calculate the average entropies `sum H(S_t)` for each feature/category `t` in `T`.
3. information gain = `H(S) - sum H(S_t)`.
    - observation: since we look for the max gain, we can just look at min avg entropy, because `H(S)` will stay the same.

### Want

- a matrix where we keep track of all the indices, so that we can do recursive groupbys
- elements should be in buckets, so we should convert Double to Int buckets
- we keep several maps around:
    - column labels, and their original indices
    - row labels (or just index numbers)
    - data buckets, so something like `Map Int [a]` where a is our original data type (generally `Double`). Or better: `Map a Int`
- last column is the classes! So we want a classification that we keep track of. Will be `String`, naively.

## How

Libraries we could have used:

- [hmatrix](http://hackage.haskell.org/package/hmatrix)
- [massiv](http://hackage.haskell.org/package/massiv)
- [accelerate](http://www.acceleratehs.org/get-started.html)
- Some "dataframes" or row type style library (Frames, vinyl, ...).
- [grids](http://hackage.haskell.org/package/grids)
- no library, just list of lists

## Resources

- data: https://archive.ics.uci.edu/ml/index.php
- `Frames` tutorial:
    - https://idontgetoutmuch.wordpress.com/2018/05/19/cartography-in-haskell/
    - https://github.com/idontgetoutmuch/psephology



