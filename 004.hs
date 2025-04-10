{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.List (find, sortBy)
{-# HLINT ignore "Use tuple-section" #-}
threeDigitNumbers =  [999,998..100]

getAllProducts = sortBy (flip compare) (map (\(x,y) -> x*y) pairs)
    where
        pairs = concatMap (\x -> map (\y -> (x,y)) threeDigitNumbers) threeDigitNumbers

isPalindrome :: Int -> Bool
isPalindrome n = a == reverse a
            where a = show n



main::IO()
main = print (find isPalindrome getAllProducts)