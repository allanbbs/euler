possibilities = [(a,b,c) | a <- [1..1000],b <- [1..1000], c <- [1..1000], a < b && b < c,a + b + c == 1000,a**2 + b**2 == c**2]

optimized = [floor (a*b*c) | a <- [1..332],
    b <- [a+1..499],
    let c = 1000 - a - b,
    a**2 + b**2 == c**2]

main  :: IO()
main = print (head optimized)