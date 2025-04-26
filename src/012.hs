import EulerUtils (reduce)
import Data.List (find)
triangleNumber n = div (n*(n+1)) 2

allDivisors :: Int -> [Int]
allDivisors n
    | n < 1 = []
    | otherwise = foldr (\d acc -> if d == n `div` d then d : acc else d : n `div` d : acc) [] divisors
        where
            divisors = [d | d <- [1..floor (sqrt (fromIntegral n))], n `mod` d == 0]



getAllDivisors = map (\x -> allDivisors x) (map (\y -> triangleNumber y) [1..])

comp x = length x > 500
main  :: IO()
main = print (find (comp) (getAllDivisors))