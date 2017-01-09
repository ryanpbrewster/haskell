module Util.PrimeTest (test_primes) where

import Test.Tasty.Discover
import Util.Prime

import qualified Data.Array as A

test_primes :: [TestTree]
test_primes =
  [ testCase "small sieve" sieve_small
  ]

sieve_small :: Assertion
sieve_small =
  let small_sieve = sieve 30
      small_primes = filter (small_sieve A.!) [2..30]
  in small_primes @?= [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
