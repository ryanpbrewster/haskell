module Util.Prime
  (bfTest
  ,divisors
  ,factors
  ,phi
  ,primes
  ,sieve
  ,sigma
  ,test)
  where

import Data.List (foldl', genericLength, group, nub)
import Data.Array (Array)
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

bfTest n = n >= 2 && bfTrialDivisionTest n 2
  where bfTrialDivisionTest n k
          | k * k > n = True
          | n `mod` k == 0 = False
          | otherwise = 
            bfTrialDivisionTest n
                                (k + 1)

-- Tests whether a given integer is prime or not
test n = n >= 2 && trialDivisionTest n primes
  where trialDivisionTest n (p:ps)
          | p * p > n = True
          | n `mod` p == 0 = False
          | otherwise = trialDivisionTest n ps

-- This primes function was taken from Richard Bird's implementation of the Sieve of Eratosthenes, as
-- declared in the epilogue of Melissa O'Neill's article "The Genuine Sieve of Eratosthenes" in J.
-- Functional Programming.
primes :: [Integer]
primes = 2 : 3 : 5 : 7 : minus (spin wheel23 11) composites
  where spin (x:xs) n = n : spin xs (n + x)
        wheel23 = cycle [2,4]
        composites = union $ map multiples primes
        multiples n = map (n *) [n ..]
        -- "minus X Y" removes all elements of y from x
        minus (x:xs) (y:ys)
          | x < y = x : minus xs (y : ys)
          | x == y = minus xs ys
          | x > y = minus (x : xs) ys
        union :: Ord a
              => [[a]] -> [a]
        union = foldr merge []
          where merge (x:xs) ys = x : merge' xs ys
                merge' (x:xs) (y:ys)
                  | x < y = x : merge' xs (y : ys)
                  | x == y = x : merge' xs ys
                  | x > y = y : merge' (x : xs) ys

sieve :: Int -> Array Int Bool
sieve bound = runSTArray $ do
  arr <- newArray (2, bound) True
  forM_ [2..bound] $ \i -> do
    is_prime <- readArray arr i
    when is_prime $ do
      forM_ [i*i, i*(i+1)..bound] $ \j -> do
        writeArray arr j False
  return arr

-- Returns a list of the factors of n
factors :: Integer -> [Integer]
factors n = factors' n primes
  where factors' n (p:ps)
          | n `mod` p == 0 = p : factors' (n `div` p) (p : ps)
          | p * p > n = if n > 1 then [n] else []
          | otherwise = factors' n ps

factorsBin n = [(head g,genericLength g)|g <- group (factors n)]

-- Returns a list of all the divisors of n Generates them using the prime factorization. This will
-- help a lot for large numbers with lots of small prime factors
divisors :: Integer -> [Integer]
divisors n = foldl' multiplyAll [1] (factorsBin n)
  where
  -- multiply all elements of xs by p^i for i <- [0..k]
  multiplyAll xs (p, k) = [ x * y | x <- xs, y <- take (k+1) (powers p) ]

powers :: Integer -> [Integer]
powers x = iterate (x*) 1

-- The DivisorSigma function. sigma k n = sum [ d^k, n `mod` d == 0 ]
sigma 0 n = product [e + 1|(_,e) <- factorsBin n]
sigma k n = 
  product [(r ^ (e + 1) - 1) `div` (r - 1)|(p,e) <- factorsBin n,let r = p ^ k]

-- just for testing purposes
sigmaBruteForce k n = sum [d ^ k|d <- [1 .. n],n `mod` d == 0]

-- Returns the number of integers k < n such that gcd(k,n) == 1
phi :: Integer -> Integer
phi n = 
  let fs = nub $ factors n
  in n `div` (product fs) * (product [f - 1|f <- fs])
