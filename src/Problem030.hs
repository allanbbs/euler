import EulerUtils (getDigits)
limit = 400000


sumOfFifthPowerDigits n p= sum $ map (\x -> x^p) (getDigits n)


main = print $ filter (\x -> (sumOfFifthPowerDigits x 5) == x) [2..limit]