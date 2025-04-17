import Prelude hiding (Left,Right)


data Direction = Right | Down | Left | Up deriving (Show,Eq)
turnRight :: Direction -> Direction
turnRight x = case x of
    Right -> Down
    Down -> Left
    Left -> Up
    Up -> Right

isDiagonal (rows,cols) (i,j) = i==j || (i+j) == (rows-1)
spiral :: Int -> Integer
spiral n = spiralAux n (0,0) Right