import Data.Set (Set, fromList, member)


allDivisors :: Integer -> [Integer]
allDivisors n
    | n < 1 = []
    | otherwise = foldr (\d acc -> if d == n `div` d then d : acc else d : n `div` d : acc) [] divisors
        where
            divisors = [d | d <- [1..floor (sqrt (fromIntegral n))], n `mod` d == 0]

sumOfDivisors :: Integer -> Integer
sumOfDivisors n = sum $ (allDivisors n)

isPerfectNumber n = sumOfDivisors n == n
isAbundant n = sumOfDivisors n > n

limit = 28123

abundantNumbers = [a | a <- [1..limit], isAbundant a]


sums = fromList [a+b | a <- abundantNumbers, b <- abundantNumbers, a + b <= limit]

main = print $ sum [a | a <- [1..limit],  not (member a sums)]