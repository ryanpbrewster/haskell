module Problems.P065 (solve) where
-- 065.hs
{-
 - What is most surprising is that the important mathematical constant,
 - e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
 -
 - Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
 -}

import Util.Math (integerDigits, fromContinuedFraction)

solve :: String
solve = show solveProblem

solveProblem = let cfrac = 2:concat [[1,2*k,1] | k <- [1..]]
                   conv = fromContinuedFraction $ take 100 cfrac
               in sum $ integerDigits $ fst conv
