rotateNum :: Integer -> [Integer]
rotateNum n = rotateNumAux (show n) b []
    where
        digits = show n
        b = length digits
        rotateNumAux _ 0 rotations = rotations
        rotateNumAux digits _ rotations = rotations
