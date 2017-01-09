module Problems.P125
  ( solve
  ) where

{-
 - The palindromic number 595 is interesting because it can be written as the
 - sum of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.
 -
 - There are exactly eleven palindromes below one-thousand that can be written
 - as consecutive square sums, and the sum of these palindromes is 4164. Note
 - that 1 = 0^2 + 1^2 has not been included as this problem is concerned with the
 - squares of positive integers.
 -
 - Find the sum of all the numbers less than 10^8 that are both palindromic and
 - can be written as the sum of consecutive squares.
 -}
import Data.List (nub, tails)

import Util.Math (integerDigits)

solve :: String
solve = show $ solveProblem (10 ^ 8)

isPalindromic n =
  let ds = integerDigits n
  in ds == reverse ds

squares = map (^ 2) [1 ..]

cumsums bound xs = takeWhile (< bound) $ tail $ scanl1 (+) xs

candidates bound =
  let all_cumsums = map (cumsums bound) (tails squares)
  in concat $ takeWhile (not . null) all_cumsums

solveProblem bound = sum $ nub $ filter isPalindromic $ candidates bound
