module Problems.P056
  ( solve
  ) where

{-
 - A googol (10^100) is a massive number: one followed by one-hundred zeros;
 - 100^100 is almost unimaginably large: one followed by two-hundred zeros.
 - Despite their size, the sum of the digits in each number is only 1.
 -
 - Considering natural numbers of the form, a^b, where a, b < 100, what is the
 - maximum digital sum?
 -}
import Util.Math (integerDigits)

solve :: String
solve = show solveProblem

solveProblem = maximum $ map digitalSum [a ^ b | a <- [1 .. 99], b <- [1 .. 99]]

digitalSum = sum . integerDigits
