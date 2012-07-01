module NumberTheory.MillerRabin (isPrime, primes) where
import NumberTheory.Core

import Control.Monad
import Control.Monad.State.Lazy
import Control.Parallel.Strategies

import Data.List
import System.Random

-- |Checks if the given number a is a witness to the compositeness of an odd n
-- by the Miller-Rabin test, assuming gcd(a, n) == 1.
isWitness :: (Integral a) => a -> a -> Bool

-- If n is prime, then Fermat's little theorem holds, so a^(n-1) = 1 (mod n)
-- for any a /= 0 (mod n). The only roots of unity mod n would be 1 and -1.
isWitness a n = isComposite intermediates
  where
    -- Let n - 1 = 2^d * k for k odd. Given n - 1, this function gives (d, k)
    getExpFactorization :: (Integral a) => a -> (Int, a)
    getExpFactorization m = if odd m
      then (0, m)
      else let (d', k') = getExpFactorization (m `div` 2) in (d' + 1, k')

    (d, k) = getExpFactorization (n - 1)

    -- A list [a^k, a^(2*k), a^(4*k), ..., a^(2^d * k)] of intermediates in the
    -- Miller-Rabin algorithm
    intermediates = take (d + 1) $ unfoldr (\seed -> Just (seed, expMod n seed 2))
        (expMod n a k)

    -- If we see any cases where the square root of 1 is neither -1 nor 1,
    -- the input is known to be composite. Otherwise, it may be composite or
    -- prime.
    isComposite (x:y:xs) = if ((x /= (n - 1)) && (x /= 1) && (y == 1))
        then True
        else isComposite (y:xs)
    isComposite [x] = if x /= 1 then True else False

-- For a number m, gets a random number in the interval (1, m) which is
-- coprime to m
randomCoprime :: (Integral a, Random a, RandomGen g) => a -> State g a
randomCoprime m = do
    gen <- get
    let (n, gen') = randomR (2, m - 1) gen
    put gen'
    if (gcd m n) == 1
        then return n
        else randomCoprime m

-- |Given a number of random coprime witnesses to test and a number,
-- yields a boolean in the (State g) monad which is False iff the number
-- is known to be composite
isPrime :: (Integral a, Random a, RandomGen g) => Int -> a -> State g Bool
isPrime numtests n = do
    -- Get numtests possible witnesses for n
    coprimes <- sequence . replicate numtests $ randomCoprime n
    if or . map (\wit -> isWitness wit n) $ coprimes
        then return False
        else return True

-- |Given how many random coprimes to choose, yields an infinite list of
-- prime numbers in the (State g) monad.
primes :: (RandomGen g) => Int -> State g [Integer]
primes numtests = do
    arePrimes <- mapM (isPrime numtests) ([3..] :: [Integer])
    return . (2:) . map fst . filter snd . zip [3..] $ using arePrimes (parTraversable rseq)
