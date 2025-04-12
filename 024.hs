import Data.List (sort)
perms :: [Char] -> [[Char]]
perms [] = [[]]
perms (x:xs) =  [pre ++ x:post | p <- perms xs, (pre,post) <- splits p]


splits :: [Char] -> [([Char],[Char])]
splits [] = [([],[])]
splits (x:xs) = ([],x:xs) : [(x:pre,post) | (pre,post) <- splits xs]

main = print ((sort (perms ['1','2','0','3','4','5','6','7','8','9']))!!999999)

