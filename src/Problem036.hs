import EulerUtils (getDigits)


palindromeBase10 n = digits == (reverse digits)
    where
        digits =  dropWhile (==0) (getDigits n)

toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

palindromeBinary n = 
    let binary = dropWhile (==0) (toBin n)
    in binary == (reverse binary)

solution = filter (\x -> (palindromeBase10 x && palindromeBinary x)) (takeWhile (<1000000) [1..])

main = print solution