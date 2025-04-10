isDivisor :: Int->Int ->Bool
isDivisor n div = mod n div == 0

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

nextFactor :: Int -> Int
nextFactor n 
    | n == 2 = 3
    | otherwise = n + 2

allPrimeDivisors :: Int -> [Int]
allPrimeDivisors n = allPrimeDivisorsAux [] 2 n
    where
        allPrimeDivisorsAux :: [Int] -> Int -> Int-> [Int]
        allPrimeDivisorsAux divisors factor n
            | n == 1 = divisors
            | mod n factor == 0 = allPrimeDivisorsAux (factor:divisors) factor (reduce n factor)
            | otherwise = allPrimeDivisorsAux divisors (nextFactor factor) n

main :: IO()
main = print (allPrimeDivisors 600851475143)





