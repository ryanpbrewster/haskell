-- Project Euler problem 14
-- Find the number, k < n, such that the Collatz sequence seeded by k is longest
--
-- Dear lord, please compile this with -O3 or higher. Otherwise it
-- is not even functional.

collatzLength 1 = 1
collatzLength n | even n = 1 + collatzLength (n `div` 2)
                | odd n  = 1 + collatzLength (3*n+1)

problem014 n = maximum $ [ (collatzLength x, x) | x <- [1..n] ]

main = print ans where ans = problem014 (10^6)
