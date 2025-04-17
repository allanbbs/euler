import Data.List (sort)
import EulerUtils (isPrime)
import Data.Char (digitToInt)

sortedDigits :: Int -> [Int]
sortedDigits n = sort $ map digitToInt (show n)

pairs = [(a, b, c) |
    a <- [1000 .. 9999-6660],
    let b = a + 3330,
    let c = a + 6660,
    let bds = sortedDigits b,
    sortedDigits a == bds && bds == sortedDigits c,
    isPrime a && isPrime b && isPrime c]