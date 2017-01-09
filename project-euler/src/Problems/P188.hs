module Problems.P188
  ( solve
  ) where

-- 188.hs
{-
 - The hyperexponentiation or tetration of a number a by a positive integer b,
 - denoted by a↑↑b or ba, is recursively defined by:
 -
 - a↑↑0 = 1,
 - a↑↑(k+1) = a^(a↑↑k).
 -
 - Thus we have e.g. 3↑↑2 = 3^3 = 27, hence 3↑↑3 = 3^27 = 7625597484987 and 3↑↑4
 - is roughly 10^(3.6383346400240996*10^12).
 -
 - Find the last 8 digits of 1777↑↑1855.
 -}
{-
 - Recall that
 -     a^(phi n) == 1 (mod n)
 -
 - Thus, consider (a ^^ (k+1)) `mod` n
 -     a ^^ (k+1) `mod` n == a^( a^^k ) `mod` n
 -                        == a^( a^^k `mod` (phi n)) `mod` n
 -}
import Util.Math (powerMod)
import Util.Prime (phi)

solve :: String
solve = show solveProblem

tetrateMod a 0 _ = 1
tetrateMod a b m =
  let t = tetrateMod a (b - 1) (phi m)
  in powerMod a t m

solveProblem = tetrateMod 1777 1855 (10 ^ 8)
