import GHC.Base (MultMul)
data Fraction = Fraction {numerator:: Integer, denominator:: Integer} 

instance Show Fraction where
  show a = show (numerator a) ++ "/" ++ show (denominator a)

instance Eq Fraction where
  (==) a b = ((numerator a == numerator b) && (denominator a == denominator b)) ||equivalent
    where
        equivalent = 
            let gcdA = gcd (numerator a) (denominator a)
                gcdB = gcd (numerator b) (denominator b)
                newA = Fraction (div  (numerator a) gcdA) (div  (denominator a) gcdA)
                newB = Fraction (div  (numerator b) gcdB) (div  (denominator b) gcdB)
                in (numerator newA == numerator newB) && (denominator newA == denominator newB)

instance Num Fraction where
  -- Define multiplication for Multiplication
  (Fraction a1 b1) * (Fraction a2 b2) = 
    Fraction (a1 * a2) (b1 * b2)
  
  -- Required Num methods (minimal implementation)
  (+) = error "Not implemented1"  -- Optional
  (-) = error "Not implemented2"  -- Optional
  abs = error "Not implemented3"  -- Optional
  signum = error "Not implemented4"  -- Optional
  fromInteger n = error "Not implemented5"  -- Optional


getDigits n = getDigitsAux n []
    where
        getDigitsAux 0 digits = digits
        getDigitsAux a digits = getDigitsAux (div a 10) (mod a 10:digits)

listToDigit :: Integral a => [a] -> a
listToDigit n = listToDigitAux (reverse n) 0 0
    where
        listToDigitAux [] _ total = total
        listToDigitAux (digit:digits) idx total = listToDigitAux digits (idx+1) (total + (digit*(10^idx))) 

removeDigit :: Fraction -> Integer -> Fraction
removeDigit fraction digit =
    let newNumerator = removeFirstItem  digit (getDigits (numerator fraction))
        newDenominator = removeFirstItem  digit (getDigits (denominator fraction))
        in Fraction (listToDigit newNumerator) (listToDigit newDenominator)

isDigitCancellingFraction :: Fraction -> Bool
isDigitCancellingFraction n = 
            let numeratorDigits = getDigits (numerator n)
            in not $ null [c | a <- numeratorDigits, let c = removeDigit n a, c == n]

allFractions = [f | a <- [10..99], b <- [10..99],mod a 10 /= 0 && mod b 10 /= 10, a < b,let f = Fraction a b, isDigitCancellingFraction f]

removeFirstItem _ []                 = []
removeFirstItem x (y:ys) | x == y    =  ys
                    | otherwise = y : removeFirstItem x ys

main = print $ denominator x
    where 
        f = foldr (*) (Fraction 1 1) allFractions
        a = numerator f
        b = denominator f
        gcdF = gcd a b
        x = if gcdF /= 1 then Fraction (div a gcdF) (div b gcdF) else f


-- Digit cancelling fractions would use a formula and pattern matching to generate the possible pais more efficiently