module NumberTheory.Pythagorean where

lattice = (1,1):genlattice (1, 1)
  where
    nlattice (1, n) = if (n `mod` 2) == 1 then (1, n+1) else (2, n-1)
    nlattice (m, 1) = if (m `mod` 2) == 1 then (m-1, 2) else (m+1, 1)
    nlattice (m, n) = if ((m + n) `mod` 2) == 0 then (m-1, n+1) else (m+1, n-1)

    genlattice (m, n) = let next = nlattice (m, n)
      in next:(genlattice next)


coprimePairs = filter (\(m,n) -> (gcd m n) == 1) lattice

pythTrips = map genTrip pairs
  where
    pairs = filter (\(s, t) -> s >= t && (s*t `mod` 2) == 1) coprimePairs
    genTrip (s, t) = (a, b, c)
        where
          a = s*t
          b = (s*s - t*t) `div` 2
          c = (s*s + t*t) `div` 2

main = mapM_ (putStrLn . show) pythTrips
