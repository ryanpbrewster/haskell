-- Project Euler problem 006
-- Evaluate (1+2+...+n)^2 - (1^2 + 2^2 + ... + n^2) for n=100

problem006 :: Integer -> Integer
problem006 n = sumsq - sqsum
               where sumsq = (n*(n+1) `div` 2)^2
                     sqsum = (n*(n+1)*(2*n+1) `div` 6)

main = print ans where ans = problem006 100
