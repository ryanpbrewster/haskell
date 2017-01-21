module Problems.P200
  ( solve
  ) where

import Data.List (isInfixOf)

-- 200.hs
{-
 - We shall define a sqube to be a number of the form, p^2q^3, where p and
 - q are distinct primes.  For example, 200 = 5^2 2^3 or 120072949 = 23^2 61^3.
 -
 - The first five squbes are 72, 108, 200, 392, and 500.
 -
 - Interestingly, 200 is also the first number for which you cannot change any
 - single digit to make a prime; we shall call such numbers, prime-proof. The
 - next prime-proof sqube which contains the contiguous sub-string "200" is
 - 1992008.
 -
 - Find the 200th prime-proof sqube containing the contiguous sub-string "200".
 -}
import Util.List (mergeInf)
import Util.Prime (primes, test)

solve :: String
solve = show $ solveProblem 199

squbes = mergeInf [[p ^ 2 * q ^ 3 | p <- primes] | q <- primes]

-- digitPlaces 12345 == [(1,10000), (2,1000), (3,100), (4,10), (5,1)]
digitPlaces n = digitPlaces' n 1
  where
    digitPlaces' 0 _ = []
    digitPlaces' n acc =
      let (q, r) = n `divMod` 10
      in (r, acc) : digitPlaces' q (10 * acc)

isPrimeProof n =
  let ds = digitPlaces n
      all_changes = [n + k * dp | (d, dp) <- ds, k <- [-d .. 9 - d]]
  in not $ any test all_changes

answers = filter isPrimeProof $ filter (isInfixOf "200" . show) squbes

solveProblem k = answers !! k
