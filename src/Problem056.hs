import EulerUtils (getDigits)

sumDigits = sum . getDigits

sums = [sumDigits $ a^b | a <- [1..99], b <- [1..99]]

main  = print $ maximum sums