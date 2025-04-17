{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import System.IO
import Text.ParserCombinators.Parsec
import Data.Char (ord)
import Data.List (sort)

quotedName :: Parser String
quotedName = do
    _ <- char '"'
    name <- many (noneOf "\"")
    _ <- char '"'
    return name

parseFile :: Parser [String]
parseFile = sepBy quotedName (char ',')

readInput :: FilePath -> IO [String]
readInput path = do
    contents <- readFile path
    case parse parseFile "" contents of
        Left err -> error (show err)
        Right names -> return names

enumerate = zip [1..]

getCharScore :: Char -> Int
getCharScore c = ord c - 64 -- Offset to 1 since A is 65  

getAlphabeticalScore :: String -> Int
getAlphabeticalScore n = sum (map getCharScore n)

getNameScore :: Int -> Int -> Int
getNameScore position alphabeticalScore = position * alphabeticalScore

calculateAllNameScores :: [String] -> Int
calculateAllNameScores names = 
    let scores = map (\(index,name) -> (getNameScore index (getAlphabeticalScore name))) (enumerate (sort names))
    in sum scores

main = do
    names <- readInput "input/022.txt"
    print (calculateAllNameScores names)

