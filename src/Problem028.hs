


sumDiagonals :: Integer -> Integer
sumDiagonals n = aux (n*n) 1 2 1 1
    where
        aux limit lastDiag currentDelta ctrl total
            | lastDiag == limit = total
            | ctrl == 5 = 
                let newDelta = currentDelta + 2
                    newDiag = lastDiag + newDelta
                in aux limit lastDiag newDelta 1 total
            | otherwise = 
                let newDiag = lastDiag + currentDelta
                in aux limit newDiag (currentDelta) (ctrl+1) (total+newDiag)


main = print $ sumDiagonals 1001