module EulerUtils where
import Data.List (nub,find)
import Data.Maybe (isNothing, fromMaybe)
import Control.Monad (forM_, when)
import Data.Array.ST (runSTUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, assocs)

isPrime :: Integral a => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n
    | n < 0 = False
    | even n = False
    | otherwise = null [d | d <- [3,5..floor (sqrt (fromIntegral n))], mod n d == 0]

sieve :: Int -> [Int]
sieve n = [p | (p, True) <- assocs (aux n)]
    where
        aux :: Int -> UArray Int Bool
        aux n = runSTUArray $ do
            arr <- newArray (2, n) True
            forM_ [2 .. floor (sqrt (fromIntegral n))] $ \p -> do
                isPrime <- readArray arr p
                when isPrime $
                    forM_ [p*p, p*p + p .. n] $ \m ->
                        writeArray arr m False
            return arr

phiSieve :: Int -> [(Int,Int)]
phiSieve n = [(a,phi) | (a, phi) <- assocs (aux n)]
    where
        aux :: Int -> UArray Int Int
        aux n = runSTUArray $ do
            arr <- newArray (1, n) 1
            forM_ [2 .. n] $ \i -> do
                writeArray arr i i
            forM_ [2..n]  $ \p -> do
                let prime = isPrime p
                when prime $ do
                    writeArray arr p (p-1)
                    forM_ [2*p,3*p .. n] $ \m -> do
                        let k = div m p
                        phiK <- readArray arr k
                        if (mod k p == 0) then writeArray arr m (phiK*(p)) else writeArray arr m (phiK*(p-1))
            return arr

reduce :: Int ->Int -> Int
reduce n factor
    | mod n factor == 0 = reduce (div n factor) factor
    | otherwise = n

isUniqueList x = isUniqueListAux x []
    where
        isUniqueListAux [] elements = True
        isUniqueListAux (x:xs) elements = notElem x elements && isUniqueListAux xs (x:elements)

fact :: Integral a => a -> Integer
fact n = aux n 1
    where
        aux :: Integral a  => a -> Integer -> Integer
        aux k total
            | k <=1 = total
            | otherwise = 
                let x = toInteger k
                in aux (x-1) (total*x)

getDigits n = getDigitsAux n []
    where
        getDigitsAux 0 digits = digits
        getDigitsAux a digits = getDigitsAux (div a 10) (mod a 10:digits)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) =  [pre ++ x:post | p <- perms xs, (pre,post) <- splits p]


splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = ([],x:xs) : [(x:pre,post) | (pre,post) <- splits xs]


listToDigit :: Num t => [t] -> t
listToDigit n = listToDigitAux (reverse n) 0 0
    where
        listToDigitAux [] _ total = total
        listToDigitAux (digit:digits) idx total = listToDigitAux digits (idx+1) (total + (digit*(10^idx))) 
