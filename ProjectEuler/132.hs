-- 132.hs
{-
 - A number consisting entirely of ones is called a repunit. We shall define
 - R(k) to be a repunit of length k.
 -
 - For example, R(10) = 1111111111 = 11×41×271×9091, and the sum of these prime
 - factors is 9414.
 -
 - Find the sum of the first forty prime factors of R(10^9).
 -}

{-
 - Observe that R(k) is divisible by p iff
 -     R(k) = 0 (mod p)
 -     (10^k - 1)/9 = 0 (mod p)
 -     10^k = 1 (mod 9*p)
 -  Just test that.
 -}

import qualified ProjectEuler.Prime as Prime
import ProjectEuler.Math (powerMod)

solveProblem count k = let fs = [ p | p <- Prime.primes, powerMod 10 k (9*p) == 1 ]
                       in sum $ take count fs

main = print $ solveProblem 40 (10^9)
