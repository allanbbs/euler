import Data.List (minimumBy, sort)
import Data.Ord (comparing)
import EulerUtils (getDigits, phiSieve)
import Data.Array (accumArray)


digitCount n = accumArray (+) 0 (0,9) [(d,1) | d <- getDigits n]
arePermutations a b = digitCount a == digitCount b

main =
  let phis = phiSieve (10 ^ 7)
      filtered = filter (\(a, phi) -> a > 10 && arePermutations a phi) phis
   in print $ fst $ minimumBy (comparing snd) [(a, k) | (a, phi) <- filtered, let k = (fromIntegral a) / (fromIntegral phi)]