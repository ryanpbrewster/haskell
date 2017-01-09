module Problems.P052
  ( solve
  ) where

import Data.List (sort)

{-
 - It can be seen that the number, 125874, and its double, 251748, contain
 - exactly the same digits, but in a different order.
 -
 - Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
 - contain the same digits.
 -}
import Util.Math (integerDigits)

solve :: String
solve = show solveProblem

solveProblem = head $ sameDigitsFamilies 6

-- The entire family has to have the same number of digits, so 10^k <= x <= n*x < 10^(k+1)
sameDigitsFamilies :: Int -> [Int]
sameDigitsFamilies n =
  let orders_of_magnitude = iterate (10 *) 1
      candidates =
        concatMap (\k -> [k .. (10 * k - 1) `div` n]) orders_of_magnitude
  in filter (familyHasSameDigits n) candidates

-- Does x start a family {i*x} where each element has the same digits for i = [1..n]
familyHasSameDigits :: Int -> Int -> Bool
familyHasSameDigits n x =
  let digits = sort $ integerDigits $ fromIntegral x
  in all
       (== digits)
       [sort $ integerDigits $ fromIntegral (i * x) | i <- [2 .. n]]
