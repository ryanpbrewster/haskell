module Problems.P124 (solve) where

import Data.List (nub, sortBy)
import Data.Ord (comparing)

import Util.Prime (factors)

solve :: String
solve = show $ solveProblem (10^5) (10^4-1)

rad = product . nub . factors

solveProblem bound k = let xs = sortBy (comparing rad) [1..bound]
                       in xs !! k
