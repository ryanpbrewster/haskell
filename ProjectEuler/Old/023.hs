-- Project Euler Problem 23
-- An abundant number is one where DivisorSigma[1, n] == 2*n
-- For instance, 28 is an abundant number, since
--     DivisorSigma[1, 28] = 1 + 2 + 4 + 7 + 14 + 28 == 2*28
-- It is helpful to think of proper divisors (divisors of n that are < n)
--
-- It is known that all numbers > 28123 can be written as the sum of two
-- abundant numbers. Find the sum of all numbers <= 28123 that CANNOT
-- be written in such a way.

import Primes_TrialDivision

abundants = [ x | x <- [2..28123], sigma1 x == 2*x ]


