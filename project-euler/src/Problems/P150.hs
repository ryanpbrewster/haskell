module Problems.P150
  ( solve
  ) where

{-
 - In a triangular array of positive and negative integers, we wish to find
 - a sub-triangle such that the sum of the numbers it contains is the smallest
 - possible.
 - 
 - In the example below, it can be easily verified that the marked triangle
 - satisfies this condition having a sum of −42.
 - 
 - We wish to make such a triangular array with one thousand rows, so we
 - generate 500500 pseudo-random numbers sk in the range ±219, using a type of
 - random number generator (known as a Linear Congruential Generator) as
 - follows:
 - 
 - t := 0
 - for k = 1 up to k = 500500:
 -     t := (615949*t + 797807) modulo 220
 -     sk := t−219
 - 
 - Thus: s1 = 273519, s2 = −153582, s3 = 450905 etc
 - 
 - Our triangular array is then formed using the pseudo-random numbers thus:
 - s1
 - s2  s3
 - s4  s5  s6 
 - s7  s8  s9  s10
 - ...
 - 
 - Sub-triangles can start at any element of the array and extend down as far
 - as we like (taking-in the two elements directly below it from the next row,
 - the three elements directly below from the row after that, and so on).
 -
 - The "sum of a sub-triangle" is defined as the sum of all the elements it
 - contains.
 -
 - Find the smallest possible sub-triangle sum.
 -}
import Data.Array.Unboxed

import Util.List (chunksBy)

solve :: String
solve = show solveProblem

solveProblem =
  let tri = take 1000 $ chunksBy [1 ..] lcg
  in minSum tri

triangleCumSums :: [[Int]] -> UArray (Int, Int) Int
triangleCumSums tri =
  let n = length tri
      cumsums = [scanl (+) 0 row | row <- tri]
      idxs = [(i, j) | i <- [1 .. n], j <- [0 .. i]]
  in accumArray (+) 0 ((1, 0), (n, n)) $ zip idxs (concat cumsums)

minSum tri =
  let n = length tri
      trics = triangleCumSums tri
  in minimum [minRootedSum trics (i, j) | i <- [1 .. n], j <- [1 .. i]]

-- the minimum subtriangle sum with the root at (i,j)
-- idealogically equivalent to:
--     minimum [ subtriangleSum (i,j) h | h <- [1..length tri - i] ]
minRootedSum trics (i, j) = minRootedSum' 0 0 0
  where
    (_, (n, _)) = bounds trics
    minRootedSum' k cm cs
      | i + k > n = cm
      | otherwise =
        let cs' = cs + trics ! (i + k, j + k) - trics ! (i + k, j - 1)
        in minRootedSum' (k + 1) (min cm cs') cs'

lcg = map (subtract (2 ^ 19)) $ tail $ iterate nextLCGTerm 0

nextLCGTerm t = ((615949 * t + 797807) `mod` (2 ^ 20))
