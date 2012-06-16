{- For a given natural number n, divide n by two if n is even, or multiply by
 - three and add one if n is odd, and repeat. For instance, the sequence we
 - get for 3 is:
 -
 - 3 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
 -
 - The Collatz conjecture states that, for any starting value of n, this
 - sequence will terminate.
 -
 - Given a maximum value N, this program searches all values n <= N for the
 - longest such Collatz chain, and yields its length and intermediate values.
 - It does so in parallel using Haskell's parallel evaluation strategies,
 - dramatically increasing its CPU utilization on multi-core machines.
 -}

import Control.Arrow
import Control.Parallel
import Control.Parallel.Strategies

import System.IO
import System.Exit
import System.Environment

import Data.List

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n = n:collatz n'
    where n' = if (n `mod` 2) == 0 then n `div` 2 else 3*n+1

usage :: String -> IO ()
usage name = putStrLn $ "Usage: " ++ name ++ " <maximum>"

main :: IO ()
main = do
    args <- getArgs
    name <- getProgName
    -- If we were passed no arguments, print usage info and exit.
    if null args then usage name >> exitWith (ExitFailure 1) else return ()
    
    let granularity = 1000
    let c = map ((length . collatz) &&& collatz) [1..(read $ head args)] :: [(Int, [Int])]

    -- Evaluate c above by computing each list element in parallel
    let col = c `using` (parListChunk granularity $ evalTuple2 rpar rdeepseq)

    let longest = let comp (l,xs) (l',xs') = if l < l'
                                                then GT
                                                else if l == l'
                                                    then EQ
                                                    else LT
                    in head . sortBy comp $ col
    putStrLn . show $ longest
