import EulerUtils (getDigits)
import Data.List (sort)


permutableDigits ns = aux (sort $ getDigits $ head ns) (tail ns)
    where
        aux digits xs
            | null xs = True
            | otherwise = if (sort $ getDigits $ head xs) == digits then aux digits (tail xs) else False

main = print $ head [a | a <- [1..],permutableDigits [a,a+a..6*a]]