module Problems.P035
  ( solve
  , rotations
  ) where

{-
 - The number, 197, is called a circular prime because all rotations of the
 - digits: 197, 971, and 719, are themselves prime.
 -
 - There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
 - 71, 73, 79, and 97.
 -
 - How many circular primes are there below one million?
 -}
import qualified Data.Array as A
import qualified Util.Math as Math
import qualified Util.Prime as Prime

solve :: String
solve = show $ solveProblem 1e6

log10 0 = 0
log10 n = 1 + log10 (n `div` 10)

rotations :: Int -> [Int]
rotations n =
  let k = log10 n
      ten = 10 ^ (k - 1)
      wrap x =
        let (q, r) = x `divMod` 10
        in ten * r + q
  in take k $ iterate wrap n

good digit = digit `elem` [1, 3, 7, 9]

solveProblem bound =
  length $ 2 : 3 : 5 : 7 : filter isCircularPrime [11,13 .. bound]
  where
    isCircularPrime n =
      all good (Math.integerDigits $ fromIntegral n) &&
      all isPrime (rotations n)
    isPrime n = sieve A.! n
    sieve = Prime.sieve bound
