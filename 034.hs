import EulerUtils (fact,getDigits)
import Data.Char (digitToInt)


main = print $ sum $ [a | a <- [1..10000000], a == (sum (map (fact . digitToInt) (show a))),a/=1,a/=2]
