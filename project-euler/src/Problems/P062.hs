module Problems.P062
  ( solve
  , solveProblem
  ) where

{-
 - The cube, 41063625 (345^3), can be permuted to produce two other cubes:
 - 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest
 - cube which has exactly three permutations of its digits which are also cube.
 -
 - Find the smallest cube for which exactly five permutations of its digits are
 - cube.
 -}
import Data.List (groupBy, sort, sortOn)

import Util.Math (integerDigits)

solve :: String
solve = show $ solveProblem 5

cubes :: [Integer]
cubes = map (^ 3) [1 ..]

powersOfTen :: [Integer]
powersOfTen = iterate (10*) 1

cubesBySize :: [[Integer]]
cubesBySize = sortIntoBins powersOfTen cubes

sortIntoBins :: Ord t => [t] -> [t] -> [[t]]
sortIntoBins (hi:ss) xs =
  let (left, right) = span (< hi) xs
  in left : sortIntoBins ss right

gatherBy :: Ord b => (a -> b) -> [a] -> [[a]]
gatherBy f xs = groupBy (\x y -> f x == f y) $ sortOn f xs

solveProblem :: Int -> Integer
solveProblem size =
  let cube_families = concatMap (gatherBy (sort . integerDigits)) cubesBySize
      good_families = filter (\fam -> length fam == size) cube_families
  in minimum $ head good_families
