
module EulerUtils where

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n
    | even n = False
    | otherwise = null [d | d <- [3,5..floor (sqrt (fromIntegral n))], mod n d == 0]

reduce :: Int ->Int -> Int
reduce n factor
    | mod n factor == 0 = reduce (div n factor) factor
    | otherwise = n