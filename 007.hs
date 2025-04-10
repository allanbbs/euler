import EulerUtils (isPrime)
main :: IO()
main = print (last (take 10000 [d | d <- [3,5..],isPrime d]))