module Problems.P024 (solve) where

{-
 - A permutation is an ordered arrangement of objects. For example, 3124 is one
 - possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
 - are listed numerically or alphabetically, we call it lexicographic order.
 - The lexicographic permutations of 0, 1 and 2 are:
 -
 - 012   021   102   120   201   210
 -
 - What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
 - 5, 6, 7, 8 and 9?
 -}

import Data.List (delete)

solve :: String
solve = show solveProblem

factorial n = product [1..n]

-- Finds the n-th permutation of distinct items
nthPermutation 0 xs = xs
nthPermutation n xs = let r = factorial (length xs - 1)
                          k = n `quot` r
                          e = xs !! k
                          n' = n - k*r
                          xs' = delete e xs
                     in e:(nthPermutation n' xs')

solveProblem = concat $ map show $ nthPermutation (1000000-1) [0..9]
