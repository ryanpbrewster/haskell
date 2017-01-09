module Problems.P048
  ( solve
  ) where

{-
 - The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
 -
 - Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
 -}
solve :: String
solve = show solveProblem

powmod _ 0 _ = 1
powmod b e m
  | even e = (x * x) `mod` m
  | odd e = (x * x * b) `mod` m
  where
    x = powmod b (e `div` 2) m

solveProblem =
  let m = 10 ^ 10
      ans = sum [powmod i i m | i <- [1 .. 1000]]
  in ans `mod` m
