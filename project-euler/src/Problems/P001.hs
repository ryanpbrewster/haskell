module Problems.P001 (solve) where

{-
 - If we list all the natural numbers below 10 that are multiples of 3 or 5,
 - we get 3, 5, 6 and 9. The sum of these multiples is 23.
 -
 - Find the sum of all the multiples of 3 or 5 below 1000.
 -}


-- Find the sum of all the multiples of {x1,x2,...} <= n


solve :: String
solve = show $ solveProblem [3,5] (1000-1)

solveProblem xs n = sum [ i | i <- [1..n], or [i `mod` x == 0 | x <- xs] ]
