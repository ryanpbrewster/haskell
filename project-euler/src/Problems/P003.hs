module Problems.P003 (solve) where

{-
 - The prime factors of 13195 are 5, 7, 13 and 29.
 -
 - What is the largest prime factor of the number 600851475143 ?
 -}

factors n = factorHelper n 2
factorHelper n k | k*k > n        = [n]
                 | n `mod` k == 0 = [k] ++ factorHelper (n `div` k) k
                 | otherwise      = factorHelper n (k+1)

largestFactor n = maximum (factors n)

solve = show $ largestFactor 600851475143
