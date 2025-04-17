nub_ :: [Integer] -> [Integer]
nub_ x = aux x []
    where
        aux [] unique = unique
        aux (x:xs) unique = if elem x unique then aux xs unique else aux xs (x:unique)
main = print $ length $ nub_ [a^b| a <-[2..100],b <-[2..100]]