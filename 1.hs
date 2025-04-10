import Distribution.Simple.Utils (xargs)

multipleSum :: Int -> Int -> Int
multipleSum currentNumber total = aux currentNumber total
    where
        aux :: Int -> Int -> Int
        aux 0 total = total
        aux x total = if (mod x 3 == 0 || mod x 5 == 0) then aux (x-1) total + x else  aux (x-1) total

main :: IO ()
main = putStrLn (show x)
    where
        x = multipleSum 999 0