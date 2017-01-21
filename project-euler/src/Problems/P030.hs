module Problems.P030
  ( solve
  ) where

import Data.List (sort)

{-
 - Surprisingly there are only three numbers that can be written as the sum of
 - fourth powers of their digits:
 -
 -     1634 = 1^4 + 6^4 + 3^4 + 4^4
 -     8208 = 8^4 + 2^4 + 0^4 + 8^4
 -     9474 = 9^4 + 4^4 + 7^4 + 4^4
 -
 - As 1 = 14 is not a sum it is not included.
 -
 - The sum of these numbers is 1634 + 8208 + 9474 = 19316.
 -
 - Find the sum of all the numbers that can be written as the sum of fifth
 - powers of their digits.
 -}
{-
 - The first thing to note is that no number over 999999 can be written as the
 - sum of fifth powers of their digits. Any 7-digit number will necessarily map
 - down to at most a 6-digit number. Thus, we need only check numbers <= 10^6
 -}
import Util.Math (integerDigits)

solve :: String
solve = show solveProblem

ordTuples
  :: Ord t
  => [t] -> Int -> [[t]]
ordTuples _ 0 = [[]]
ordTuples xs k =
  let tups = ordTuples xs (k - 1)
  in [x : t | t <- tups, x <- xs, null t || x <= head t]

legit :: [Integer] -> Bool
legit tup =
  let ans = sum $ map (^ 5) tup
  in tup == sort (integerDigits ans)

solveProblem :: Integer
solveProblem =
  let sols = filter legit $ concat [ordTuples [0 .. 9] k | k <- [2 .. 6]]
  in sum [sum $ map (^ 5) sol | sol <- sols]
