-- Project Euler problem 003
-- Find the largest prime factor of 600,851,475,143 == 600851475143

factorsGreaterThanK :: Integer -> Integer -> [Integer]
factorsGreaterThanK n k | n == 1         = []
                        | k*k > n        = [n]
                        | n `mod` k == 0 = k : factorsGreaterThanK (n `div` k) k
                        | otherwise      = factorsGreaterThanK n $ k+1

factors :: Integer -> [Integer]
factors n = factorsGreaterThanK n 2

problem003 n = maximum $ factors n

main = print ans where ans = problem003 600851475143
