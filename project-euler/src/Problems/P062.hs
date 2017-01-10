module Problems.P062
  ( solve
  ) where

{-
 - The cube, 41063625 (345^3), can be permuted to produce two other cubes:
 - 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest
 - cube which has exactly three permutations of its digits which are also cube.
 -
 - Find the smallest cube for which exactly five permutations of its digits are
 - cube.
 -}
import Data.List (groupBy, sort, sortBy)
import Data.Ord (comparing)

import Util.Math (integerDigits)

solve :: String
solve = show $ solveProblem 5

cubes :: [Integer]
cubes = map (^ 3) [1 ..]

cubesBySize :: [[Integer]]
cubesBySize = sortIntoBins [10 ^ n | n <- [0 ..]] cubes

sortIntoBins :: Ord t => [t] -> [t] -> [[t]]
sortIntoBins (hi:ss) xs =
  let (left, right) = span (< hi) xs
  in left : sortIntoBins ss right

gatherBy :: (t -> t -> Ordering) -> [t] -> [[t]]
gatherBy cmp xs = groupBy (\x y -> cmp x y == EQ) $ sortBy cmp xs

solveProblem :: Int -> Integer
solveProblem size =
  let cmp = comparing (sort . integerDigits)
      cube_families = concatMap (gatherBy cmp) cubesBySize
      good_families = filter (\fam -> length fam == size) cube_families
  in minimum $ head good_families
