import EulerUtils (isPrime)
import Data.Ord (comparing)
import Data.List (maximumBy)

generateQuadraticFunction :: Integer -> Integer -> (Integer -> Integer)
generateQuadraticFunction a b x = (toInteger x)^2 + a*(toInteger x) + b

possibleCoefs :: [(Integer,Integer)]
possibleCoefs = [(a,b) | a <- [-999,-997..999], b <- [1..1000], isPrime b]

countConsecutivePrimes :: (Integer -> Integer) -> Integer
countConsecutivePrimes f = go 0
  where
    go n = if isPrime (f n) then go (n+1) else n


allQuadraticFormulaPrimes = [[a,b,countConsecutivePrimes (generateQuadraticFunction a b)] | (a,b) <- possibleCoefs]

main = print prod
    where
        mostPrimesTrie = maximumBy (comparing (!!2)) allQuadraticFormulaPrimes
        prod = head mostPrimesTrie * mostPrimesTrie!!1

-- This is a great problem. Instead of relying on clever data 
-- structures or algorithms (the computations are simple, the problem is the state space)
-- it relies on reasoning and understanding of the problem to constrain the state space to make it solvable