-- 132.hs
{-
 - A number consisting entirely of ones is called a repunit. We shall define
 - R(k) to be a repunit of length k.
 -
 - For example, R(10) = 1111111111 = 11×41×271×9091, and the sum of these prime
 - factors is 9414.
 -
 - Find the sum of the first forty prime factors of R(109).
 -}

{-
 - We know that A(p) is the smallest value of k such that p | R(k).
 - Thus, p | R(10^9) iff A(p) | 10^9
 - That is, if A(p) == k' (such that p | R(k')) then k must be a multiple of k'
 -}

import qualified ProjectEuler.Prime as Prime

-- (a n) stolen from 129.hs. Look there.
a n = aH n 1 1
aH n 0 k = k
aH n r k = let r' = (10*r+1) `mod` n
           in aH n r' (k+1)

solveProblem count k =
    let fs = filter (\p -> k `mod` (a p) == 0) (drop 3 Prime.primes)
    in take count fs

main = print $ solveProblem 40 (10^9)
