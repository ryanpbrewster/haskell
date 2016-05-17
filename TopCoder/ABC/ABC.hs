-- ABC.hs
{-
You are given two s: N and K. Lun the dog is interested in strings that satisfy
the following conditions:

The string has exactly N characters, each of which is either 'A', 'B' or 'C'.
The string s has exactly K pairs (i, j) (0 <= i < j <= N-1) such that s[i] < s[j].
If there exists a string that satisfies the conditions, find and return any
such string. Otherwise, return an empty string.
-}

import Data.List (tails)
import Control.Monad (replicateM)

type Problem = (Int, Int)
type Solution = String

verify :: Problem -> Solution -> Bool
verify (n, k) str = backout str == (n, k)

backout :: Solution -> Problem
backout str = (length str, sum $ map backout' $ tails str)
  where
  backout' [] = 0
  backout' (x:xs) = length $ filter (>x) xs

solve :: Problem -> [Solution]
solve = bruteForce

bruteForce :: Problem -> [Solution]
bruteForce (n, k) = filter (verify (n,k)) (replicateM n "ABC")

dfs (n0, k0) = map reverse $ dfs' n0 k0 (0,0)
  where
  dfs' 0 k _ = if k == 0 then [""] else []
  dfs' n k (as, bs)
    | k < 0 = []
    | k == allAs n (as, bs) = [ replicate n 'A' ]
    | k == allBs n (as, bs) = [ replicate n 'B' ]
    | k == allCs n (as, bs) = [ replicate n 'C' ]
    | otherwise =
      map ('A':) (dfs' (n-1) k (as+1, bs)) ++
      map ('B':) (dfs' (n-1) (k - as) (as, bs+1)) ++
      map ('C':) (dfs' (n-1) (k - as - bs) (as, bs))
  allAs n (as, bs) = 0
  allBs n (as, bs) = n * as
  allCs n (as, bs) = n * (as + bs)
  maxPossible n (as, bs) = ???
