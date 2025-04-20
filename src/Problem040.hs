import EulerUtils
digits = concatMap (getDigits) [1..10^7]
indexes = [1,10,100,1000,10000,100000,1000000]

main = print $ product [digits!!(i-1) | i <- indexes]