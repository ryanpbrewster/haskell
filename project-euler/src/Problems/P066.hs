module Problems.P066
  ( solve
  , solveProblem
  ) where

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

import Debug.Trace
import Util.List (maximumBy)
import Data.Ratio
import Data.List (find, foldl1, inits, unfoldr)
import Data.Maybe (fromJust)

solve = show $ solveProblem 1000

solveProblem :: Integer -> Integer
solveProblem bound = maximumBy (fst . minimalSolution) ([1..bound] `minus` squares)

minimalSolution :: Integer -> (Integer, Integer)
minimalSolution d =
  let cfrac = sqrtContinuedFraction d
      candidates = [ (numerator v, denominator v) | v <- map fromContinuedFraction $ tail $ inits cfrac ]
  in fromJust $ find (\(x,y) -> x*x - d*y*y == 1) candidates

squares = map (^2) [1..]

minus xs [] = xs
minus [] _ = []
minus (x:xs) (y:ys)
  | x < y  = x : minus xs (y:ys)
  | x > y  = minus (x:xs) ys
  | x == y = minus xs ys


fromContinuedFraction :: [Integer] -> Ratio Integer
fromContinuedFraction = foldr1 (\a b -> a + 1 / b) . map (%1)

-- sqrt(n) = 
sqrtContinuedFraction :: Integer -> [Integer]
sqrtContinuedFraction n =
  unfoldr (Just . nextFraction) (1, 0, 1)
  where
  x = sqrt (fromIntegral n)
  -- (b*sqrt(n) + c) / d
  --  --> 1 / ( (b*sqrt(n) + c)/d - a )
  --   == d / ( b*sqrt(n) + c - a*d ) [ let e = c - a*d ]
  --   == d * (b*sqrt(n) - e) / (b*sqrt(n) + e) * (b*sqrt(n) - e)
  --   == (d*b * sqrt(n) - d*e) / (b*b*n - e*e)
  nextFraction (b, c, d) =
    let a = floor $ (x * fromIntegral b + fromIntegral c) / fromIntegral d
        e = c - a * d
        b' = d * b
        c' = -d * e
        d' = b * b * n - e * e
        g = foldl1 gcd [b', c', d']
    in (a, (b' `div` g, c' `div` g, d' `div` g))
