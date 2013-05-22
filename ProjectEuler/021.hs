-- 021.hs
-- Project Euler problem 21
--
-- Find the sum of all amicable numbers less than n, where n = 10,000
-- An amicable number is defined by the divisor sum function,
-- sigma1(k) = Sum[d, d divides k]
-- An amicable number is a member of an amicable pair
-- An amicable pair is a pair of numbers, (a,b), such that
-- sigma1(a) == b, and sigma1(b) == a


import qualified ProjectEuler.Prime as Prime

sig n = (Prime.sigma 1 n) - n
problem021 n = sum [a+b| a <- [1..n],
                         let b = sig a,
                         a > b,
                         b > 0,
                         sig b == a]

main = print ans where ans = problem021 10000
