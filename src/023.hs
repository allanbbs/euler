import Data.Set (Set, fromList, member)
sumOfDivisors :: Integer -> Integer
sumOfDivisors n = sum [a | a <- [1..(div n 2)],mod n a == 0]

isPerfectNumber n = sumOfDivisors n == n
isAbundant n = sumOfDivisors n > n

limit = 28123

abundantNumbers = [a | a <- [1..limit], isAbundant a]
set = fromList abundantNumbers

x = [a+b | a <- abundantNumbers, b <- abundantNumbers, a <= b, (a+b) <= limit]

main = print result
    where
        result = sum [x | x <- [1..limit], not (member x set)]