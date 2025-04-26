import EulerUtils (fact)

comb n r = div (fact n) ((fact r) * (fact (n - r)))


allCombs n = aux n 1 []
    where
        aux k r combs
            | r > n = combs
            | otherwise = 
                let value = comb k r
                    in aux k (r+1) (value:combs)

combsGreaterThan n c =length $ filter (>c) (allCombs n)


main = print $ sum [combsGreaterThan a 1000000 | a <-[1..100]]