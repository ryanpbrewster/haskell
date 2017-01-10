module Problems.P025
  ( solve
  ) where

{-
 -
 - The Fibonacci sequence is defined by the recurrence relation:
 -     F[n] = F[n−1] + F[n−2], where F[1] = 1 and F[2] = 1.
 -
 - The 12th term, F[12], is the first term to contain three digits.
 -
 - What is the first term in the Fibonacci sequence to contain 1000 digits?
 -}
solve :: String
solve = show solveProblem

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

solveProblem :: Int
solveProblem = idxWithDigits 1000 fibs

idxWithDigits d xs = length $ takeWhile (\x -> length (show x) < d) xs

firstWithDigits d xs = head $ dropWhile (\x -> length (show x) < d) xs
