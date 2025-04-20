import Data.Ord (comparing)
import Data.List (maximumBy)

primitiveTriples :: [(Int, Int, Int)]
primitiveTriples = 
    [ (m^2 - n^2, 2*m*n, m^2 + n^2)
    | m <- [2..floor (sqrt 1000)],
      n <- [1..m-1],
      gcd m n == 1,
      odd (m + n)
    ]

allTriples :: [(Int, Int, Int)]
allTriples = concatMap scaleTriples primitiveTriples
  where
    scaleTriples (a,b,c) = takeWhile (\(x,y,z) -> x+y+z <= 1000) 
                          [(k*a, k*b, k*c) | k <- [1..]]

countSolutions :: [(Int, Int)]
countSolutions = 
    [ (p, length ts) 
    | p <- [1..1000],
      let ts = filter (\(a,b,c) -> a + b + c == p) allTriples,
      not $ null ts
    ]    

main = print $ maximumBy (comparing snd) countSolutions