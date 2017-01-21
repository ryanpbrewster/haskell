module Problems.P066
  ( solve
  , solveProblem
  ) where

import Data.List (find)
import Data.Maybe (fromJust)

{-
 - Consider quadratic Diophantine equations of the form:
 -
 - x^2 – D*y^2 = 1
 -
 - For example, when D=13, the minimal solution in x is 649^2 – 13×180^2 = 1.
 -
 - It can be assumed that there are no solutions in positive integers when D is
 - square.
 -
 - By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
 - following:
 -
 - 32 – 2×22 = 1
 - 22 – 3×12 = 1
 - 92 – 5×42 = 1
 - 52 – 6×22 = 1
 - 82 – 7×32 = 1
 -
 - Hence, by considering minimal solutions in x for D ≤ 7, the largest x is
 - obtained when D=5.
 -
 - Find the value of D ≤ 1000 in minimal solutions of x for which the largest
 - value of x is obtained.
 -}
import Util.List (maximumBy)

solve = show $ solveProblem 1000

solveProblem :: Integer -> Integer
solveProblem bound =
  maximumBy (fst . minimalSolution) ([1 .. bound] `minus` squares)

minimalSolution :: Integer -> (Integer, Integer)
minimalSolution d =
  fromJust $
  find (\(x, y) -> y > 0 && x * x - d * y * y == 1) (sqrtConvergents d)

sqrtConvergents :: Integer -> [(Integer, Integer)]
sqrtConvergents n = explore (0, 1) (1, 0)
  where
    explore (a, b) (e, f) =
      let (c, d) = (a + e, b + f)
      in (c, d) :
         if c * c < n * d * d
           then explore (c, d) (e, f)
           else explore (a, b) (c, d)

squares = map (^ 2) [1 ..]

minus xs [] = xs
minus [] _ = []
minus (x:xs) (y:ys)
  | x < y = x : minus xs (y : ys)
  | x > y = minus (x : xs) ys
  | x == y = minus xs ys
