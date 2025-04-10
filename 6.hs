squareOfNsum n = floor (a**2)
    where a = (n*(n+1))/2


sumOfNSquares n = floor(sum (map (** 2) [1..n]))


n = 100
main :: IO()
main = print (squareOfNsum n - sumOfNSquares n)