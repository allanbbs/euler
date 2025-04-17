sumOfDivisors n = sum [a | a <- [1..(div n 2)],mod n a == 0]
d = sumOfDivisors

pairs = [(a,b)| a <-[1..10000],let b = d a, a < b, d b == a]

flatten :: [(Int,Int)] -> [Int]
flatten [] = []
flatten [(x,y)] = [x,y]
flatten (x:xs) =
    let (a,b) = x
    in a:b:flatten xs
main = print result
    where
        result = sum (flatten pairs)