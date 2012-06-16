{- |Tests Fermat's little theorem on composite numbers in parallel, printing
 - out Carmichael numbers -}
import NumberTheory.Core
import Control.Parallel.Strategies
import System.IO

granularity = 100 -- How many list elements to evaluate in parallel?

{- Predicate which determines if a composite integer is a Carmichael number,
 - using data parallel evaluation -}
carmichael :: (Integral a) => a -> Bool
carmichael m = let ns = map (\a -> a^(m-1) `mod` m) . coprimes $ m
                 in all (==1) (ns `using` parBuffer granularity rseq)

{- Prints out each Carmichael number on its own line -}
main = do
    let cars = (filter carmichael composites :: [Integer])
    hSetBuffering stdout LineBuffering
    mapM_ (putStrLn . show) cars
