import Data.List (find)

minDivisor = 10
maxDivisor = 20
divisors = [minDivisor..maxDivisor]



dividesByAll :: Int -> Bool
dividesByAll n = dividesByAll' divisors n
    where
        dividesByAll' [] x = True
        dividesByAll' (l:ls) x = mod x l == 0 && dividesByAll' ls x


main :: IO()
main = print (find dividesByAll [maxDivisor,maxDivisor*2..])