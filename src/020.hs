import Data.Char (digitToInt)
sumDigits n = sum (map digitToInt (show n))


fact :: Int -> Integer
fact n
    | n <=1 = 1
    | otherwise = toInteger n * fact(n-1)


main:: IO()
main = print (sumDigits (fact 100))