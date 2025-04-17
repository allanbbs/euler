import EulerUtils (sieve)

isPerfectSquare n = sq * sq == n
    where
        sq = floor (sqrt (fromIntegral n :: Double))
candidates = [3,5..]

goldbatchConjecture :: Integer -> Bool
goldbatchConjecture n =
    let primes = sieve n
        in
            or [isPerfectSquare (div (n-p) 2) | p <- primes]

main = print (head [a | a<-[3,5..],not (goldbatchConjecture a)])

