import EulerUtils (reduce)
import Data.List (find)
triangleNumber n = sum [1..n]

allDivisors :: Int -> [Int]
allDivisors n = allDivisorsAux [] 1 n
    where
        allDivisorsAux :: [Int] -> Int -> Int-> [Int]
        allDivisorsAux divisors factor n
            | factor == n = factor:divisors
            | mod n factor == 0 = allDivisorsAux (factor:divisors) (factor+1) n
            | otherwise = allDivisorsAux divisors (factor + 1) n



getAllDivisors = map (\x -> allDivisors x) (map (\y -> triangleNumber y) [1..])

comp x = length x > 500
main  :: IO()
main = print (find (comp) (getAllDivisors))