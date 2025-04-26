import Data.Set (fromList, member)
import Data.Foldable (minimumBy)

pentagonal n = div (n*(3*n - 1)) 2

pentagonalsList = [pentagonal a | a <- [1..3000]]

set = fromList pentagonalsList


candidates = [diff | a <- pentagonalsList,
    b <- pentagonalsList,
    a < b,
    let diff = abs (a-b),
    let s = a + b,
    member diff set,
    member s set
    ]


main = print $ minimum candidates