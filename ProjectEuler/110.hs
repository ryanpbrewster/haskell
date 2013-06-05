-- 110.hs
{-
 - In the following equation x, y, and z are positive integers.
 - 1/x + 1/y == 1/z
 -
 - It can be verified that when n = 1260 there are 113 distinct solutions and
 - this is the least value of n for which the total number of distinct
 - solutions exceeds one hundred.
 -
 - What is the least value of n for which the number of distinct solutions
 - exceeds four million?
 -
 - NOTE: This problem is a much more difficult version of problem 108 and as it
 - is well beyond the limitations of a brute force approach it requires
 - a clever implementation.
 -}

{-
 - See 108.hs for an explanation of the code
 -}

import ProjectEuler.Prime (sigma, primes)
import ProjectEuler.Math (fullCoinCombos)

parts = fullCoinCombos [1..]

expsToInt :: Integral a => [a] -> Integer
expsToInt = product . zipWith (^) primes

ndivs = sigma 0
numSolutions z = 1 + (ndivs (z^2)) `div` 2

solveProblem bound =
    let candidates = concat $ [ map expsToInt ps | ps <- parts ]
    in minimum $ take 100 $ filter (\z -> numSolutions z > bound) candidates

main = print $ solveProblem (4*10^6)
