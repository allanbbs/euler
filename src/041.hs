import EulerUtils (getDigits,isUniqueList,perms,isPrime)
import Data.List (find,sort, drop)

digits :: [Integer]
digits = [1,2,3,4,5,6,7,8,9]

trimDigits :: [Integer] -> [Integer]
trimDigits n
    | last n == 0 = take (length n - 1) n
    | head n == 0 =  reverse (drop 1 (reverse n))
    | otherwise = n

toDigit digits = aux (reverse (trimDigits digits)) 0 0
    where
        aux [] number _= number
        aux (digit:digits) number idx= aux digits (number + digit*(10^idx)) (idx+1)

getCandidates digits =  reverse $ sort $ map toDigit (perms digits)

isPandigital :: Integral a =>  a -> Int -> Bool
isPandigital n d =
    let digits = map (fromIntegral) (getDigits n)
    in (length digits == d) && isUniqueList digits && (sort digits == [1..d])


findSolution :: [Integer] -> Maybe Integer
findSolution ds = solutionAux ds
    where
        solutionAux [] = Nothing
        solutionAux digits = case filter (\x -> (isPrime) x && (isPandigital x (length digits))) (getCandidates digits) of
            (p:ps) -> Just p
            [] -> solutionAux (take (length digits -1) digits)



main = print $  findSolution digits