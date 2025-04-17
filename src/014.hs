import Data.List (sortBy, maximumBy)
import Data.Function (on)
import Prelude hiding (lookup)

import Control.Monad.
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as Map

collatzSequence :: Int -> Int
collatzSequence n = collatzAux n 1
    where
        collatzAux 1 elements = elements
        collatzAux x elements =
            let newNumber = if even x then  div x 2 else 3*x + 1
            in collatzAux newNumber (elements + 1)

collatzSequenceManualMemo :: Int -> Int
collatzSequenceManualMemo n = fst $ collatzAuxMemo n Map.empty
    where
        collatzAuxMemo :: Int -> IntMap Int -> (Int, IntMap Int)
        collatzAuxMemo 1 known  = (1,known)
        collatzAuxMemo x known =
                case Map.lookup x known of
                    Just knownLen -> (knownLen,known)
                    Nothing -> 
                        let newValue = if even x then  div x 2 else 3*x + 1
                            (len,newKnown) = collatzAuxMemo newValue known
                            newLen = len + 1
                        in (newLen,Map.insert x newLen newKnown)


collatzSequence :: Int -> Int
collatzSequence = memo collatz
  where
    collatz 1 = 1
    collatz n = 1 + collatz next
      where
        next = if even n then n `div` 2 else 3 * n + 1


problem = [(x,collatzSequence x) | x <- [1..999999]]
main :: IO()
main = print (maximumBy (on compare snd) problem)