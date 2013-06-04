-- 062.hs
{-
 - The cube, 41063625 (345^3), can be permuted to produce two other cubes:
 - 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest
 - cube which has exactly three permutations of its digits which are also cube.
 -
 - Find the smallest cube for which exactly five permutations of its digits are
 - cube.
 -}

import ProjectEuler.Math (integerDigits)
import Data.List (groupBy, sort, sortBy)
import Data.Ord (comparing)

cubes = map (^3) [1..]
cubesBySize = sortIntoBins [10^n | n <- [0..]] cubes

sortIntoBins (hi:ss) xs = let (left,right) = span (<hi) xs
                          in left : sortIntoBins ss right

gatherBy cmp xs = groupBy (\x -> \y -> cmp x y == EQ) $ sortBy cmp xs

generalProblem size = let cmp = comparing (sort.integerDigits)
                          cube_families = concat $ map (gatherBy cmp) cubesBySize
                          good_families = filter (\fam -> length fam == size) cube_families
                      in minimum $ head good_families

solveProblem = generalProblem 5

main = print solveProblem
