fib :: Int -> Int
fib n = fibAux 0 1 n
    where
        fibAux :: Int -> Int -> Int -> Int
        fibAux a _ 0 = a
        fibAux a b n = fibAux b (a+b) (n-1)


fibsUnder :: Int -> [Int]
fibsUnder limit = takeWhile (<= limit) (map fib [0..])


problem2 :: Int -> Int
problem2 limit = sum (filter even (fibsUnder limit))


main :: IO()
main = putStrLn (show (problem2 4000000))