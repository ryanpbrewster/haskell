-- Project Euler problem 004
-- Find the largest palindromic product of 3-digit numbers


problem004 :: Integer -> Integer
problem004 n = let lo = 10^(n-1)
                   hi = 10^n
               in maximum [ x | a <- [lo..hi], b <- [a..hi],
                               let x = a*b, (show x) == (reverse $ show x) ]

main = print ans where ans = problem004 3
