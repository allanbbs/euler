import EulerUtils (isPrime)
reduce :: Int ->Int -> (Int,Int)
reduce n factor = aux n factor 0
    where
        aux x f k
            | mod x f == 0 = aux (div x f) f (k+1)
            | otherwise = (x,k)

nextFactor :: Int -> Int
nextFactor n 
    | n == 2 = 3
    | otherwise = n + 2

allPrimeDivisors :: Int -> [(Int,Int)]
allPrimeDivisors n = allPrimeDivisorsAux [] 2 n
    where
        allPrimeDivisorsAux :: [(Int,Int)] -> Int -> Int-> [(Int,Int)]
        allPrimeDivisorsAux divisors factor n
            | n == 1 = divisors
            | mod n factor == 0 = 
                let (f,k) = reduce n factor 
                    in allPrimeDivisorsAux ((factor,k):divisors) factor f
            | otherwise = allPrimeDivisorsAux divisors (nextFactor factor) n

isUniqueList :: Eq a => [a] -> Bool
isUniqueList xs = aux xs []
    where
        aux [] _ = True
        aux (h:t) seen = if elem h seen then False else aux t (h:seen)

generateConsecutives = [(a,a+1,a+2,a+3) | a <- [1..]]

solution = take 1 [(a,b,c,d) | (a,b,c,d) <- generateConsecutives,
    not (isPrime a),
    not (isPrime b),
    not (isPrime c),
    not (isPrime d),
    let aDivs = allPrimeDivisors a,
    let bDivs = allPrimeDivisors b,
    let cDivs = allPrimeDivisors c,
    let dDivs = allPrimeDivisors d,
    length aDivs == 4,
    length bDivs == 4,
    length cDivs == 4,
    length dDivs == 4,
    isUniqueList (aDivs ++ bDivs ++ cDivs ++ dDivs)]


