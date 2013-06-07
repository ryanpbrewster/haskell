-- 130.hs
{-
 - A number consisting entirely of ones is called a repunit. We shall define
 - R(k) to be a repunit of length k; for example, R(6) = 111111.
 -
 - Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
 - there always exists a value, k, for which R(k) is divisible by n, and let
 - A(n) be the least such value of k; for example, A(7) = 6 and A(41) = 5.
 -
 - You are given that for all primes, p > 5, that p − 1 is divisible by A(p).
 - For example, when p = 41, A(41) = 5, and 40 is divisible by 5.
 -
 - However, there are rare composite values for which this is also true; the
 - first five examples being 91, 259, 451, 481, and 703.
 -
 - Find the sum of the first twenty-five composite values of n for which GCD(n,
 - 10) = 1 and n − 1 is divisible by A(n).
 -}

import qualified ProjectEuler.Prime as Prime

-- "a n" is taken from 129.hs. Look there for explanation.
a n = aH n 1 1
aH n 0 k = k
aH n r k = let r' = (10*r+1) `mod` n
           in aH n r' (k+1)


legit n = (not $ Prime.test n) && ((n-1) `mod` (a n) == 0)

solveProblem count = let candidates = filter (\n -> gcd n 10 == 1) [3,5..]
                     in sum $ take count $ filter legit candidates

main = print $ solveProblem 25
