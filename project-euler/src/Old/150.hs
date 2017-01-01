-- 150.hs
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

import ProjectEuler.Util (chunksBy)
import Data.Array
import Data.List (minimumBy, minimum)
import Data.Ord (comparing)

main = print solveProblem

solveProblem = let tri = take 1000 $ chunksBy [1..] lcg
                   extractSum (idx,n,s) = s
               in minimumBy (comparing extractSum) $ subtriangleSums tri

triangleCumSums tri = let n = length tri
                          cumsums = [ scanl (+) 0 row | row <- tri ]
                          idxs = [ (i,j) | i <- [1..n], j <- [0..i] ]
                      in accumArray (+) 0 ((1,0),(n,n)) $ zip idxs (concat cumsums)

subtriangleSums tri = let n = length tri
                          trics = triangleCumSums tri
                      in concat [ rootedSubtriangleSums trics (i,j) | i <- [1..n], j <- [1..i] ]

rootedSubtriangleSums trics (i,j) =
    let (_, (n,_)) = bounds trics
        row_sums = [ trics ! (i+k,j+k) - trics ! (i+k,j-1) | k <- [0..n-i] ]
        cum_sums = scanl1 (+) row_sums
    in [ ((i,j),k,rs) | (k,rs) <- zip [1..n-i+1] cum_sums ]

lcg = map (subtract (2^19)) $ tail $ iterate nextLCGTerm 0
nextLCGTerm t = ((615949*t + 797807) `mod` (2^20))

test = chunksBy [1..] [1..55]
