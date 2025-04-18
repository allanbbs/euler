import System.IO
import Text.ParserCombinators.Parsec
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as Map
import EulerUtils
import Data.Char (ord)


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


triangle :: Int -> Int
triangle n = div (n*(n+1)) 2

triangleMap = mapAux 100000 Map.empty
    where
        mapAux 1 m = Map.insert 1 1 m
        mapAux n currentMap = mapAux (n-1) (Map.insert  (triangle n) n currentMap)


wordValue :: [Char] -> Int
wordValue word = sum $ map (subtract 64 . ord) word

findTriangleWords :: [String] -> Int
findTriangleWords words = aux words []
    where
        aux [] triangleWords = length triangleWords
        aux (word:remainingWords) triangleWordsSoFar =
            case Map.lookup (wordValue word) triangleMap of
                Just num -> aux remainingWords (word:triangleWordsSoFar)
                Nothing -> aux remainingWords triangleWordsSoFar


main = do
    words <- readInput "input/042.txt"
    print (findTriangleWords words)
