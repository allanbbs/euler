import Data.ByteString (count)
import Control.Exception (throw)
numberToText :: (Eq a, Num a) => a -> String
numberToText x
    | x == 0 = ""
    | x == 1 = "one"
    | x == 2 = "two"
    | x == 3 = "three"
    | x == 4 = "four"
    | x == 5 = "five"
    | x == 6 = "six"
    | x == 7 = "seven"
    | x == 8 = "eight"
    | x == 9 = "nine"
    | x == 10 = "ten"
    | x == 11 = "eleven"
    | x == 12 = "twelve"
    | x == 13 = "thirteen"
    | x == 14 = "fourteen"
    | x == 15 = "fifteen"
    | x == 16 = "sixteen"
    | x == 17 = "seventeen"
    | x == 18 = "eighteen"
    | x == 19 = "nineteen"
    | x == 20 = "twenty"
    | x == 30 = "thirty"
    | x == 40 = "forty"
    | x == 50 = "fifty"
    | x == 60 = "sixty"
    | x == 70 = "seventy"
    | x == 80 = "eighty"
    | x == 90 = "ninety"
    | otherwise = ""

numberAsText x
    | x == 1000 = "one thousand"
    | x >= 100 =
        let hundreds = div x 100
            a =  numberToText  hundreds ++ " hundred"
            remainder = mod x 100
            remainderPart = if remainder > 0 then numberAsText remainder else ""
        in a ++ (if remainder == 0 then "" else " and ") ++ remainderPart
    | x > 20 = numberToText ((div x 10)*10) ++ " " ++ numberToText (mod x 10)
    | otherwise = numberToText x


countLetters :: String -> Int
countLetters a = length (filter (\x -> x /=' ')  a)

countAllLetters :: [String] -> Int
countAllLetters a = sum (map countLetters a)


main::IO()
main = print (countAllLetters [numberAsText x | x <- [1..1000]])
