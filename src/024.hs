import Data.List (sort)
import EulerUtils (perms)

main = print ((sort (perms ['1','2','0','3','4','5','6','7','8','9']))!!999999)