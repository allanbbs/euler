import Data.Char (digitToInt)
sumDigits n = sum (map digitToInt (show n))


main :: IO()
main = print (sumDigits (2^1000))