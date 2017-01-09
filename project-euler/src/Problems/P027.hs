module Problems.P027
  ( solve
  ) where

import Util.List (maximumBy)
{-
 - Euler published the remarkable quadratic formula:
 -     n^2 + n + 41
 -
 - It turns out that the formula will produce 40 primes for the consecutive
 - values n = 0 to 39. However, when n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41 is
 - divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly
 - divisible by 41.
 -
 - Using computers, the incredible formula  n^2 − 79n + 1601 was discovered,
 - which produces 80 primes for the consecutive values n = 0 to 79. The product
 - of the coefficients, −79 and 1601, is −126479.
 -
 - Considering quadratics of the form:
 -
 - n^2 + an + b, where |a| < 1000 and |b| < 1000
 -
 - where |n| is the modulus/absolute value of n
 - e.g. |11| = 11 and |−4| = 4
 -
 - Find the product of the coefficients, a and b, for the quadratic expression
 - that produces the maximum number of primes for consecutive values of n,
 - starting with n = 0.
 -}
{-
- Clearly b must be prime, since
-     n == 0 --> n^2 + an + b == b,
- so to even get past n=0 we must have b be prime
- Then note that
-     n == 1 --> 1 + a + b must be prime
-            --> a + b + a > 1 (since all primes are > 1)
-            --> a > -b
- so we can run a from (-b,999).
-}
import qualified Util.Prime as Prime

solve :: String
solve = show solveProblem

bound = 999

solveProblem =
  let primes = takeWhile (<= bound) Prime.primes
      coeffs = [(a, b) | b <- primes, a <- [-b .. bound]]
      (a, b) = maximumBy consecutivePrimes coeffs
  in a * b

type Coefficients = (Integer, Integer) -- coefficients are pairs (a, b)

consecutivePrimes :: Coefficients -> Int
consecutivePrimes (a, b) =
  length $ takeWhile Prime.test [b + n * (a + n) | n <- [0 ..]]
