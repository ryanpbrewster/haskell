module Problems.P010
  ( solve
  ) where

import Data.Array ((!))

{-
 - The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 - Find the sum of all the primes below two million.
 -}
import qualified Util.Prime as Prime

solve :: String
solve = show $ solveProblem 2e6

solveProblem bound = sum $ filter isPrime [2 .. bound]
  where
    isPrime n = sieve ! n
    sieve = Prime.sieve bound
