-- 014.hs
-- Project Euler problem 14
-- Find the number, k < n, such that the Collatz sequence seeded by k is longest
--
-- Dear lord, please compile this with -O3 or higher. Otherwise it
-- is not even functional.


collatzLength n = cl n 0

cl 1 c = c+1
cl n c | even n = cl (n `quot` 2) (c+1)
       | odd n  = cl (3*n+1)      (c+1)

problem014 n = snd $ maximum $ [ (collatzLength x, x) | x <- [1..n] ]

main = print ans where ans = problem014 (10^6)
