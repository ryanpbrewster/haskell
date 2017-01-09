module Problems.P044
  ( solve
  ) where

{-
 - Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. The first ten
 - pentagonal numbers are:
 -
 - 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
 -
 - It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference,
 - 70 − 22 = 48, is not pentagonal.
 -
 - Find the pair of pentagonal numbers, Pj and Pk, for which their sum and
 - difference are pentagonal and D = |Pk − Pj| is minimised; what is the value
 - of D?
 -}
import Data.List (inits)

solve :: String
solve = show solveProblem

pent :: Integer -> Integer
pent i = i * (3 * i - 1) `div` 2

pents :: [Integer]
pents = map pent [1 ..]

isPent :: Integer -> Bool
isPent k =
  let n = round $ (1.0 + sqrt (1.0 + 24.0 * (fromIntegral k))) / 6.0
  in pent n == k

sols =
  [ (x, x')
  | xs <- tail $ inits pents
  , let x' = last xs
  , x <- init xs
  , isPent (x' - x) && isPent (x' + x)
  ]

solveProblem =
  let (x, x') = head $ sols
  in x' - x