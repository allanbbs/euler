import Data.MemoTrie (memo2)

countWays :: [Integer] -> Integer -> Integer
countWays = memo2 go
    where
        go _ 0 = 1
        go [] _ = 0
        go (c:cs) amount
            | c > amount = go cs amount
            | otherwise = go (c:cs) (amount - c) + go cs amount


coins = [1,2,5,10,20,50,100,200]

main = print $ countWays coins 200