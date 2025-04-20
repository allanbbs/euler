import EulerUtils (getDigits, listToDigit, generatePrimes, isPrime)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as Map


truncateLeft n ds= 
        let truncs = scanl (\x y -> x*10 + y) 0 ds
        in tail truncs
truncateRight n ds = aux ds 1 [n]
    where
        aux digits idx nums
            | idx == length ds = nums
            | otherwise = 
                let number = listToDigit (drop idx digits)
                in aux digits (idx +1) (number:nums)

solution = take 11 [a | a <- generatePrimes, a > 7,let ds = getDigits a,notElem 0 ds, all (isPrime) (truncateRight a ds), all (isPrime) (truncateLeft a ds)]
main = print solution
