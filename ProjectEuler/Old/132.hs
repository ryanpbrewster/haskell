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

import qualified ProjectEuler.Prime as Prime

{-
 - Take advantage of Fermat's little theorem:
 - If p is a prime then
 -     a^(p-1) == 1 (mod p)
 - Let a = 10 and we get
 -     10^(p-1) == 1 (mod p)
 - Given that R(k) = 10^0 + 10^1 + ... + 10^(k-1)
 - we then have a cycle (of length p-1), mod p
 -     R(k) `mod` p = (10^0 `mod` p) + ... + (10^(p-2) `mod` p) + (10^(p-1) `mod` p) + ...
 -                  = 1 + r[1] + ... + r[p-2] + 1 + r[1] + ...
 -                    \--------------------/
 -                        ~k/(p-1) times
 -}

-- repunitMod k p == R(k) `mod` p
repunitMod k p =
    let rems = iterate (\x -> (10*x) `mod` p) 1
        rem_cycle = 1 : takeWhile (/= 1) (tail rems)
        cycle_len = length rem_cycle
        rem_cycle_sum = (sum rem_cycle) `mod` p
        num_cycles = (k-1) `quot` cycle_len
        leftovers = k - num_cycles*cycle_len
        total = (rem_cycle_sum*num_cycles + (sum $ take leftovers rem_cycle))
    in total `mod` p

solveProblem count k = let ps = map fromIntegral (drop 3 Prime.primes)
                           fs = filter (\p -> repunitMod k p == 0) ps
                       in sum $ take count fs

main = print $ solveProblem 40 (10^9)
