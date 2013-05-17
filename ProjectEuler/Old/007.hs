-- Project Euler problem 007
-- Find Prime[10001]


-- noDivisors n L = any x in L such that x divides n
noDivisors n (d:ds) | d*d > n        = True
                    | n `mod` d == 0 = False
                    | otherwise      = noDivisors n ds

primes = 2:[ x | x <- [3,5..], isPrime x]
isPrime n = noDivisors n primes

problem007 n = primes !! (n-1)

main = print ans where ans = problem007 10001
