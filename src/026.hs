import Data.List (elemIndex)
import Data.List (sortOn)
solution :: Int -> [Int]
solution 1 = [1]
solution n = reverse $ aux n (mod 1 n) [] []
    where
        aux :: Int -> Int -> [Int] -> [Int] -> [Int]
        aux x current decimals mods
            | current == 0 = decimals
            | current `elem` mods  = decimals
            | otherwise =
                let
                    n = current * 10
                    nextQuotient = div n x
                    nextMod = mod n x
                    hasMod = elem nextMod mods
                in aux x nextMod (nextQuotient:decimals) (current:mods)


allDigits = sortOn snd [(d,length $ solution d) | d <- [1..999]]

main = print $ last allDigits

-- There is an alternative approach using multiplicative order mod, this is a very specific mathematical insight
-- but the implementation is pretty simple


