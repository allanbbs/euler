import Prelude hiding (lookup)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.List (find)

countDigits :: Integer -> Integer
countDigits 0 = 1
countDigits x
    | x < 10 = 1
    | otherwise = 1 + countDigits (div x 10)


fibAux :: Int -> IntMap Integer -> (Integer, IntMap Integer)
fibAux 1 known = (1,known)
fibAux 2 known = (2,known)
fibAux n known =
    case Map.lookup n known of
        Just value -> (value,known)
        Nothing ->
            let (v1,known1) = fibAux (n-1) known
                (v2,known2) = fibAux (n-2) known1
                newValue = v1 + v2
            in (newValue,Map.insert n newValue known2)

fib :: Int -> Integer
fib n = fst $ fibAux n Map.empty

main = print (head [n | n <- [1..], countDigits (fib n) == 1000] + 1)