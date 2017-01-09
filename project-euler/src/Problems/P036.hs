module Problems.P036
  ( solve
  ) where

{-
 - The decimal number, 585 = 10010010012 (binary), is palindromic in both
 - bases.
 -
 - Find the sum of all numbers, less than one million, which are palindromic in
 - base 10 and base 2.
 -
 - (Please note that the palindromic number, in either base, may not include
 - leading zeros.)
 -}
{-
 - One small optimization: only odd numbers can be palindromic in base 2
 -}
import Util.Math (reverseDigitsBy)

solve :: String
solve = show $ solveProblem 1e6

solveProblem bound = sum $ filter legit [1,3 .. bound]

legit n = reverseDigitsBy 10 n == n && reverseDigitsBy 2 n == n
