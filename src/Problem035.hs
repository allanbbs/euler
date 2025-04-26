import Data.Set (Set, fromList, member)
import EulerUtils (isPrime, sieve)

rotate :: Int -> [Int]
rotate n = aux (show n) (show n) []
  where
    aux current initial rotations
      | elem initial rotations = filter (/= n) (map read rotations :: [Int])
      | otherwise =
          let nextRotation = (tail current) ++ [head current]
           in aux nextRotation initial (nextRotation : rotations)

primes = sieve 1000000

primeSet = fromList primes

allPrimeRotations n set= all (`member` set) (rotate n)

main = print $ length [a | a <- primes, allPrimeRotations a primeSet]
