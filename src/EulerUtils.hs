module EulerUtils where
import Data.List (nub,find)
import Data.Maybe (isNothing, fromMaybe)

isPrime :: Integral a => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n
    | n < 0 = False
    | even n = False
    | otherwise = null [d | d <- [3,5..floor (sqrt (fromIntegral n))], mod n d == 0]

reduce :: Int ->Int -> Int
reduce n factor
    | mod n factor == 0 = reduce (div n factor) factor
    | otherwise = n

isUniqueList x = isUniqueListAux x []
    where
        isUniqueListAux [] elements = True
        isUniqueListAux (x:xs) elements = notElem x elements && isUniqueListAux xs (x:elements)

fact :: Integral a => a -> Integer
fact n
    | n <=1 = 1
    | otherwise = toInteger n * fact (n-1)

getDigits n = getDigitsAux n []
    where
        getDigitsAux 0 digits = digits
        getDigitsAux a digits = getDigitsAux (div a 10) (mod a 10:digits)



sieve :: Integer -> [Integer]
sieve n = takeWhile (<= n) primes
    where
        primes :: [Integer]
        primes = aux [2..]
            where
                aux (p:xs) = p : aux [x | x <- xs, x `mod` p /= 0]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) =  [pre ++ x:post | p <- perms xs, (pre,post) <- splits p]


splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = ([],x:xs) : [(x:pre,post) | (pre,post) <- splits xs]
