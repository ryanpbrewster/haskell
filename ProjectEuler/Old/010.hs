-- Project Euler problem 10
-- Find the sum of all the primes < n, where n = 2,000,000
--
-- NOTE: The definition in Primes is very inefficient.
--       At some point you should update it to use some
--       better data structures
import Primes_TrialDivision

problem010 n = sum $ takeWhile (< n) primes

main = print ans where ans = problem010 $ 2*10^6
