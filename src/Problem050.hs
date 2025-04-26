import EulerUtils (sieve, isPrime)
import Data.List (find, sortBy)
import Data.Ord (comparing)

limit = 4000
primes = sieve limit

candidates = [(rangeSum,limit - (b+a)) |
    a <- [1..div limit 2],
    b <- [1..div limit 2],
    a < b,
    let rangeSum = sum $ drop (fromInteger b) (reverse $ drop (fromInteger a) primes)]

main = print $ find (\(n,l) -> isPrime n && n < 1000000) (reverse $ sortBy (comparing snd) candidates)