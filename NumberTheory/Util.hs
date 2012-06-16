module NumberTheory.Util where

{- Ascending list difference function, which will allow us to create a list of
 - composite numbers by removing the prime numbers from the list [4..]. Because
 - the inputs are in ascending order, we can do this lazily, and on infinite
 - lists. -}
ascDiff :: (Ord a) => [a] -> [a] -> [a]
ascDiff (x:xs) (y:ys)
    | x == y    = ascDiff xs ys
    | x < y     = x:ascDiff xs (y:ys)
    | x > y     = ascDiff (x:xs) ys
ascDiff xs [] = xs
ascDiff [] _ = []
