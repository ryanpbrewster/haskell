module Problems.P023
  ( solve
  ) where

import Data.Array
-- 023.hs Uses a slightly different strategy than 023_first.hs
--
-- Instead of constructing all the possible sums first, then summing over the false elements, let's
-- just check each element as we go. This way there are far fewer array operations.
--
-- The memoization using the array speeds this up by a factor of ~45x
import qualified Util.Prime as Prime

solve :: String
solve = show solveProblem

bound = 23123

solveProblem = sum $ filter (not . canMake) [1 .. bound]

-- Memoize the answer for [1..bound]
is_abundant = listArray (1, bound) $ map isAbundant [1 .. bound]
  where
    isAbundant n = Prime.sigma 1 n > 2 * n

isAbundantMemo n = is_abundant ! n

abundants = filter isAbundantMemo [1 .. bound]

-- To see if we can make n as a sum of two abundant numbers, k and k', simply construct all possible
-- k' (as n-k for k \in abundants) and see if any of them are abundant numbers
canMake n = any isAbundantMemo [n - k | k <- takeWhile (< n) abundants]
