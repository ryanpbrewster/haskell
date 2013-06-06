import ProjectEuler.Prime (factors)
import Data.List (nub, sortBy)
import Data.Ord (comparing)

rad = product . nub . factors

solveProblem bound k = let xs = sortBy (comparing rad) [1..bound]
                       in xs !! k

main = print $ solveProblem (10^5) (10^4-1)
