module Problems.P069
  ( solve
  , solveProblem
  ) where

import Util.Prime (primes)

solve :: String
solve = show $ solveProblem 1e6

solveProblem :: Integer -> Integer
solveProblem bound = last $ takeWhile (< bound) $ scanl lcm 1 primes
