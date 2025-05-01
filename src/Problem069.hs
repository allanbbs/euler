import EulerUtils(phiSieve)
import Data.Array.Unboxed (UArray, assocs)
import Data.Array.ST (runSTUArray, newArray, readArray, writeArray)
import Data.Foldable (forM_, maximumBy)
import Control.Monad (when)
import Data.Ord (comparing)


main = print $ fst $ maximumBy (comparing snd) [(a,k) | (a,phi) <- phiSieve 1000000, let k = (fromIntegral a :: Double)/(fromIntegral phi :: Double)]