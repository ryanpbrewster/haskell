module Problems.P183 (solve) where

{-
 - Let N be a positive integer and let N be split into k equal parts, r = N/k,
 - so that N = r + r + ... + r.  Let P be the product of these parts,
 - P = r × r × ... × r = r^k.
 -
 - For example, if 11 is split into five equal parts, 11 = 2.2 + 2.2 + 2.2
 - + 2.2 + 2.2, then P = 2.2^5 = 51.53632.
 -
 - Let M(N) = Pmax for a given value of N.
 -
 - It turns out that the maximum for N = 11 is found by splitting eleven into
 - four equal parts which leads to Pmax = (11/4)^4; that is, M(11) = 14641/256
 - = 57.19140625, which is a terminating decimal.
 -
 - However, for N = 8 the maximum is achieved by splitting it into three equal
 - parts, so M(8) = 512/27, which is a non-terminating decimal.
 -
 - Let D(N) = N if M(N) is a non-terminating decimal and D(N) = -N if M(N) is
 - a terminating decimal.
 -
 - For example, ΣD(N) for 5 ≤ N ≤ 100 is 2438.
 -
 - Find ΣD(N) for 5 ≤ N ≤ 10000.
 -}

{-
 - M(N) = max (N/k)^k for any k
 - In general, this occurs at k = N/e (where e = 2.718...)
 - Thus, let k = round (N/e) and M(N) = (N/k)^k
 -
 - Second note: M(N) will form a terminating decimal ONLY if the denominator
 - has no prime factors other than 2 and 5 (see terminatingDecimal). However,
 - (N/k)^k and (N/k) have exactly the same prime factors in the denominator, so
 - we can check the much smaller denominator without losing anything.
 -}

import Data.Ratio
import Data.List

import Util.Prime (factors)

solve :: String
solve = show solveProblem

e :: Double
e = exp 1.0

terminatingDecimal d = null $ (nub $ factors d) \\ [2,5]

m n = let k = round ((fromIntegral n)/e)
      in (n%k)^k

-- m' is just for computational convenience
m' n = let k = round ((fromIntegral n)/e)
       in (n%k)

d n = if terminatingDecimal (denominator $ m' n) then (-n) else n

solveProblem = sum $ map d [5..10000]
