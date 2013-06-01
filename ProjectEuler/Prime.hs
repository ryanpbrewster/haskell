module ProjectEuler.Prime
( test
, primes
, factors
, divisors
, sigma
, phi
) where

import Data.List (group, nub)

-- Tests whether a given integer is prime or not
test n | n < 2     = False -- 0 and 1 are not prime, nor are negative numbers
       | n == 2    = True
       | otherwise = trialDivisionTest n 2

trialDivisionTest n k | n `mod` k == 0 = False
                      | k*k > n        = True
                      | otherwise      = trialDivisionTest n (k+1)


{-
 - This primes sieve was taken from Richard Bird's implementation of the
 - Sieve of Eratosthenes, as declared in the epilogue of Melissa O'Neill's
 - article "The Genuine Sieve of Eratosthenes" in J. Functional Programming.
 -}
primes :: [Integer]
primes = 2:minus [3,5..] composites
    where composites = union [ multiples p | p <- primes ]
multiples n = map (n*) [n..]

-- "minus X Y" removes all elements of y from x
minus (x:xs) (y:ys) | x<y  = x:minus xs     (y:ys)
                    | x==y =   minus xs     ys
                    | x>y  =   minus (x:xs) ys

union :: Ord a => [[a]] -> [a]
union = foldr merge []
    where merge (x:xs) ys = x:merge' xs ys
          merge' (x:xs) (y:ys) | x < y  = x:merge' xs (y:ys)
                               | x == y = x:merge' xs ys
                               | x > y  = y:merge' (x:xs) ys


-- Returns a list of the factors of n
factors n = factors' n primes
    where factors' n (p:ps) | n `mod` p == 0 = p:factors' (n `quot` p) (p:ps)
                            | p*p > n        = if n > 1 then [n] else []
                            | otherwise      = factors' n ps


factorsBin n = let fs = factors n
                   -- fs_grouped will be a list of grouped factors
                   -- fs == [5, 3, 3, 3, 2, 2]
                   -- --> fs_grouped = [ [5], [3,3,3], [2,2] ]
                   fs_grouped = group fs
               in [ (head g, fromIntegral $ length g) | g <- fs_grouped ]

-- Returns a list of all the divisors of n
-- Generates them using the prime factorization. This will help a lot
-- for large numbers with lots of small prime factors
divisors n = let fs_bin = factorsBin n
             in divisors' fs_bin

divisors' [] = [1]
divisors' (x:xs) = let (f,c) = x -- the factor, and its associated count
                       fs = take (c+1) $ iterate (f*) 1 -- fs = [1,f,f^2,...,f^c]
                       divs = divisors' xs
                   in [ f' * d | d <- divs, f' <- fs ]


-- The DivisorSigma function. sigma k n = sum [ d^k, n `mod` d == 0 ]
sigma 0 n = let fs_bin = factorsBin n in product [ e+1 | (_,e) <- fs_bin ]
sigma k n = let fs_bin = factorsBin n
            in product [ (r^(e+1)-1) `quot` (r-1) | (p,e) <- fs_bin, let r = p^k ]

-- just for testing purposes
sigmaBruteForce k n = sum [ d^k | d <- [1..n], n `mod` d == 0 ]


-- Returns the number of integers k < n such that gcd(k,n) == 1
phi n = let fs = nub $ factors n
        in n `div` (product fs) * (product $ map (subtract 1) fs)
