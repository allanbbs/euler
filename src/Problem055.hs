import EulerUtils (getDigits, listToDigit)


isPalindrome n = 
    let digits = getDigits n
        in digits == (reverse digits)

producesPalindrome n = aux n 0
    where
        aux k it
            | it > 50 = False
            | it > 1 && isPalindrome k = True
            | otherwise =
                let 
                    digits = getDigits k
                    newNumber = (listToDigit digits) + (listToDigit $ reverse digits)
                in if isPalindrome newNumber then True else aux newNumber (it+1)

isLychrel n = not $ producesPalindrome n


main = print $ length $ filter (isLychrel) [1..10000]
