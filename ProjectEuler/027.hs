-- 027.hs
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

import qualified ProjectEuler.Prime as Prime
import ProjectEuler.Util (tuples)
import Data.Ord (comparing)
import Data.List (maximumBy)

main = print solveProblem
solveProblem = let coeffs = tuples 2 [-999..999]
                   [ba, bb] = bestCoeffs coeffs
               in [ba,bb]

consecutivePrimes a b = length $ takeWhile good [ n*n + a*n + b | n <- [0..] ]
    where good = (\v -> v > 0 && Prime.test v)

bestCoeffs coeffs = let f = (\[a,b] -> consecutivePrimes a b)
                        vals = map f coeffs
                        best = maximumBy (comparing snd) $ zip coeffs vals
                    in fst best
