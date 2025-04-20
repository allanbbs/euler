import Data.List (sort, sortBy)
import EulerUtils (getDigits)
import Data.Maybe (isNothing, fromMaybe)
import Data.Ord (comparing)
import Debug.Trace (trace)

isPandigitalMultiple n multiples = 
    let digits = sort$ getProductConcat n multiples
    in length digits == 9 && digits == [1..9]

getProductConcat n multiples = aux n multiples []
    where
        aux _ [] digits = digits
        aux n (multiple:multiples) digitsSoFar = aux n multiples  (digitsSoFar ++ getDigits (n*multiple))

isPandigital n =
    let digits = sort $ (getDigits n)
    in digits == [1..9]


listToDigit :: Num t => [t] -> t
listToDigit n = listToDigitAux (reverse n) 0 0
    where
        listToDigitAux [] _ total = total
        listToDigitAux (digit:digits) idx total = listToDigitAux digits (idx+1) (total + (digit*(10^idx))) 

findLargestPandigitalMultiple = aux 9 []
    where
        aux :: Int -> [Int] -> Maybe Int
        aux n numbers
            | n == 1 = if not (null numbers) then Just $ last (sort numbers) else Nothing
            | otherwise = 
                let candidates = takeWhile (\x -> (length (fst x) <= 9)) [(ds, listToDigit ds) | a <- [1..],let ds = getProductConcat a [1..n]]
                    pandigitals = (sortBy (comparing snd) (filter (isPandigital . snd) candidates))
                    k = map (snd) pandigitals
                   in if null pandigitals then aux (n-1) numbers else aux (n-1) (k ++ numbers) 