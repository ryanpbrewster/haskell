-- Project Euler problem 21
--
-- Find the sum of all amicable numbers less than n, where n = 10,000
-- An amicable number is defined by the divisor sum function,
-- sigma1(k) = Sum[d, d divides k]
-- An amicable number is a member of an amicable pair
-- An amicable pair is a pair of numbers, (a,b), such that
-- sigma1(a) == b, and sigma1(b) == a


import Primes_TrialDivision hiding (sigma, sigma1)

divisors k = [ d | d <- [1..k], k `mod` d == 0 ]

-- sigma1_O0 k: directly find and sum all the divisors of k
sigma1_O0 k = sum $ divisors k
sigma1_O0_proper k = (sigma1_O0 k) - k

-- sigma1_O1 k:
--     Factor k as p1^e1 * p2^e2 * ... * pr^er
--     We want to find the sum of
--     1 + p1 + p2 + ... + p1*p2 + p1*p3 + ...
--       + p1*p2^2 + p1*p3^2 + ...
--       + ...
--       + p1^2*p2 + p1^2*p3 + ...
--       + p1^2  + ...
--     = (1+p1+p1^2+...+p1^e1)*(1+p2*p2^2+...+p2^e2)*...
--  A simpler way to put it is:
--      Product[Sum[p_i^j, {j,1,e_i}], {i,1,r}]
--  Observe further that Sum[p_i^j, {j,1,e_i}] can be simplified to
--      (1-p_i^(e_i+1))/(1-p_i) = (p_i^(e_i+1) - 1)/(p_i - 1)
--  So we really want
--      Product[ (p_i^(e_i+1) - 1)/(p_i - 1), {i,1,r} ]
sigma1_O1 k = let fsbin = factors_bin k
              in product [ (p^(e+1)-1) `div` (p-1) | (p,e) <- fsbin ]

sigma1_O1_proper k = (sigma1_O1 k) - k

sigma1 = sigma1_O1_proper

problem021 n = sum [a+b| a <- [1..n], let b = sigma1 a, a > b, b > 0, sigma1 b == a]

main = print ans where ans = problem021 10000


-- Just as a little something extra, let's do the general DivisorSigma
-- All that we do is calculate (1 + p^2 + p^4 + ...) instead of (1+p+p^2+...)
-- if we want sigma2, and in general (1 + p^m + p^(2m) + ...) = ((p^m)^(e+1)-1)/(p^m-1)
sigma_O1 m k = let fsbin = factors_bin k
               in product [ ((p^m)^(e+1)-1) `div` (p^m-1) | (p,e) <- fsbin ]
