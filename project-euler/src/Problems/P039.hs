module Problems.P039
  ( solve
  ) where

import Data.List (sort, group)
import Util.List (maximumBy)
{-
 - If p is the perimeter of a right angle triangle with integral length sides,
 - {a,b,c}, there are exactly three solutions for p = 120.
 -
 - {20,48,52}, {24,45,51}, {30,40,50}
 -
 - For which value of p <= 1000, is the number of solutions maximised?
 -}
import Util.Math (primitiveTriples)

solve = show $ solveProblem 1000

solveProblem bound =
  let prims = takeWhile (\t -> sum t < bound) primitiveTriples -- take the relevant primitive triples
      trips = [map (k *) t | t <- prims, k <- [1 .. bound `div` sum t]] -- generate the non-primitives
      perims = map sum trips
  in head $ maximumBy length $ group $ sort perims
