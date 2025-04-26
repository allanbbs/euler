triangle n = div (n * (n + 1)) 2

pentagonal n = div (n * (3 * n - 1)) 2

hexagonal n = n * (2 * n - 1)

integerSqrt :: Integer -> Integer
integerSqrt n = floor (sqrt (fromIntegral n :: Double))

sol a b c = 
    let disc = b*b -4*a*c
        root = integerSqrt disc
        in root*root==disc && mod (-b + root) (2*a) == 0

triangleSol k = sol 1 1 (-2*k)
pentagonalSol k = sol 3 (-1) (-2*k)
hexagonalSol k = sol 2 (-1) (-k)

main = print $ [h | a <- [1..],
    let h = hexagonal a,
    triangleSol h,
    pentagonalSol h]