import EulerUtils (getDigits,perms)
import Data.List (sort)

isPandigital n =
    let digits = sort $ (getDigits n)
    in digits == [0..9]

listToDigit :: Num t => [t] -> t
listToDigit n = listToDigitAux (reverse n) 0 0
    where
        listToDigitAux [] _ total = total
        listToDigitAux (digit:digits) idx total = listToDigitAux digits (idx+1) (total + (digit*(10^idx))) 

getPandigitalSlices n = aux (getDigits n) 1 []
    where
        aux digits idx slices
            | idx == 8 = slices
            | otherwise = aux digits (idx+1)  ((listToDigit [digits!!a | a <- [idx,idx+1,idx+2]]):slices)

matchesSolution n = aux (getPandigitalSlices n) [17,13,11,7,5,3,2]
    where
        aux slices primes
            | null slices = True
            | mod (head slices) (head primes) == 0 = aux (tail slices) (tail primes)
            | otherwise = False

candidates = map (listToDigit) (perms [0,1,2,3,4,5,6,7,8,9])

main = print $ sum [a | a <- candidates,isPandigital a && matchesSolution a]