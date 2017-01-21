module Problems.P032
  ( solve
  ) where

import Data.List (sort, nub)

{-
 - We shall say that an n-digit number is pandigital if it makes use of all the
 - digits 1 to n exactly once; for example, the 5-digit number, 15234, is
 - 1 through 5 pandigital.
 -
 - The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
 - multiplicand, multiplier, and product is 1 through 9 pandigital.
 -
 - Find the sum of all products whose multiplicand/multiplier/product identity
 - can be written as a 1 through 9 pandigital.  HINT: Some products can be
 - obtained in more than one way so be sure to only include it once in your
 - sum.
 -}
import Util.Math (integerDigits)

solve :: String
solve = show solveProblem

uniqueDigits n =
  let digits = integerDigits n
  in nub digits == digits

solveProblem = sum $ nub solutions
  where
    solutions =
      concatMap
        pandigitalProducts
        [([1 .. 9], [1000 .. 9999]), ([10 .. 99], [100 .. 999])]
    pandigitalProducts (as, bs) =
      [ a * b
      | a <- filter uniqueDigits as
      , b <- filter uniqueDigits bs
      , isPandigitalProduct a b
      ]

isPandigitalProduct a b =
  let digits = integerDigits a ++ integerDigits b ++ integerDigits (a * b)
  in sort digits == [1 .. 9]
