module NumberTheory.Core where
import NumberTheory.Util

-- |Inefficient but easy definition for the prime numbers
primes :: (Integral a) => [a]
primes = let s (x:xs) = x:s [n | n <- xs, (n `mod` x) > 0] in s [2..]


{- Function to, for an integer m, compute all coprime numbers less than m, which
 - is to say, the list of numbers m in [1..n] for which gcd(m,n) = 1 -}
coprimes :: (Integral a) => a -> [a]
coprimes m = [n | n <- [1..m-1], gcd m n == 1]

{- Infinite list of the composite numbers -}
composites :: (Integral a) => [a]
composites = ascDiff [4..] primes

-- |Performs exponentiation mod n, using repeated squaring.
expMod :: (Integral a) => a -> a -> a -> a
expMod _ _ 0 = 1
expMod n a m = if even m then evenCase else oddCase
  where
    evenCase = (expMod n a (m `div` 2))^2 `mod` n
    oddCase = a*(expMod n a (m - 1)) `mod` n
