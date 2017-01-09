module Problems.P190
  ( solve
  ) where

-- 190.hs
{-
 - Let Sm = (x1, x2, ... , xm) be the m-tuple of positive real numbers with x1 + x2 + ... + xm = m for which Pm = x1 * x22 * ... * xmm is maximised.
 -
 - For example, it can be verified that [P10] = 4112 ([ ] is the integer part function).
 -
 - Find Σ[Pm] for 2 ≤ m ≤ 15.
 -}
solve :: String
solve = show solveProblem

solveProblem = sum [floor (p m) | m <- [2 .. 15]]

p m =
  let lambda = fromIntegral (m + 1) / 2
  in product [(fromIntegral i / lambda) ^ i | i <- [1 .. m]]
