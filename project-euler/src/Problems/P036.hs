module Problems.P036
  ( solve
  ) where

import Util.List (imerge, palindromes)

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
 - We can directly construct palindromes in each base. Then we just find the common elements.
 -}
import Util.Math (fromIntegerDigitsBy)

solve :: String
solve = show $ solveProblem 1e6

solveProblem bound = sum $ takeWhile (< bound) (multibasePalindromes [2, 10])

multibasePalindromes [] = []
multibasePalindromes bases = foldr1 imerge (map basePalindromes bases)

basePalindromes b =
  map (fromIntegerDigitsBy b) $
  filter (\xs -> head xs > 0) $ palindromes [0 .. b - 1]
