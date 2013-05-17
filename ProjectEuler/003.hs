-- 003.hs
{-
 - The prime factors of 13195 are 5, 7, 13 and 29.
 -
 - What is the largest prime factor of the number 600851475143 ?
 -}

factors n = factorHelper n 2
factorHelper n k | k*k > n        = [n]
                 | n `mod` k == 0 = [k] ++ factorHelper (n `div` k) k
                 | otherwise      = factorHelper n (k+1)

solveProblem n = last $ factors n

main = print $ solveProblem 600851475143
