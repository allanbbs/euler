import EulerUtils
main :: IO()
main = print (sum (takeWhile (< 2000000) [d | d <- [1..],isPrime d]))