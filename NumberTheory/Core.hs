module NumberTheory.Core where
import NumberTheory.Util

{- Function to, for an integer m, compute all coprime numbers less than m, which
 - is to say, the list of numbers m in [1..n] for which gcd(m,n) = 1 -}
coprimes :: (Integral a) => a -> [a]
coprimes m = [n | n <- [1..m-1], gcd m n == 1]

-- |Performs exponentiation mod n, using repeated squaring.
expMod :: (Integral a) => a -> a -> a -> a
expMod _ _ 0 = 1
expMod n a m = if even m then evenCase else oddCase
  where
    evenCase = (expMod n a (m `div` 2))^2 `mod` n
    oddCase = a*(expMod n a (m - 1)) `mod` n
