module Problems.P493
  ( solve
  , solveProblem
  ) where

import Data.Ratio
import Numeric (fromRat, showFFloat)

{-
 - 70 colored balls are placed in an urn, 10 for each of the seven rainbow
 - colors.
 -
 - What is the expected number of distinct colors in 20 randomly picked balls?
 -
 - Give your answer with nine digits after the decimal point (a.bcdefghij).
 -}
{-
 - Suppose we have c colors, each with n_i balls.
 - Let x_i = 1 if color i is present, 0 otherwise.
 - The answer we're looking for is the expected value,
 -   E[x_1 + ... + x_c]
 -
 - All the x_i functions are independent, so
 -   E[x_1 + ... + x_c] = Sum[E[x_i], {i, 1, c}]
 -
 - If there are M = Sum[n_i, {i, 1, c}]  total balls, and we're picking t of them, then
 - there are
 -   M `choose` t
 - total picks, and
 -   (M - n_i) `choose` t
 - of them do not contain any balls of color i, so
 -   E[x_i] = 1 - (M - n_i) `choose` t / M `choose` t
 - and thus
 -   E[x_1 + ... + x_c] = Sum[1 - (M - n_i) `choose` t / M `choose` t, {i, 1, c}]
 -}
import Util.Math (binomial)

solve :: String
solve = showFFloat (Just 9) (fromRat $ solveProblem (replicate 7 10) 20) ""

-- suppose there are `c` colors, each with `n` balls
-- what is the expected number of colors when picking `t` balls
solveProblem :: [Integer] -> Integer -> Rational
solveProblem ns t =
  let m = sum ns
  in sum [1 % 1 - (m - ni) `choose` t % m `choose` t | ni <- ns]

choose = binomial
