import Data.List (nub)
data Multiplication = Multiplication{a:: Integer,b :: Integer, result :: Integer} deriving (Show)


instance Eq Multiplication where
  (==) a b = result a == result b

getDigits n = getDigitsAux n []
    where
        getDigitsAux 0 digits = digits
        getDigitsAux a digits = getDigitsAux (div a 10) (mod a 10:digits)

isUniqueList x = isUniqueListAux x []
    where
        isUniqueListAux [] elements = True
        isUniqueListAux (x:xs) elements = notElem x elements && isUniqueListAux xs (x:elements)

isPandigital :: Multiplication -> Bool
isPandigital n =
    let a1 = getDigits (a n)
        b1 = getDigits (b n)
        r1 = getDigits (result n)
        allDigits = a1 ++ b1 ++ r1
    in (length allDigits == 9) && isUniqueList allDigits && sum allDigits == 45


pairs = [Multiplication a b (a*b) | 
    a <- [1..9999],
    b <- [1..9999],
    a < b,
    a /= b,
    a*b<=9999,
    isUniqueList (getDigits a),
    isUniqueList (getDigits b)
    ]

allPandigitals = nub $ filter (isPandigital) pairs
main = print $ sum $ map (\x -> result x) allPandigitals