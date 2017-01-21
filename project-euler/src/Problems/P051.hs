module Problems.P051
  ( solve
  ) where

import Data.Array
import qualified Util.Math as Math

{-
 - By replacing the 1st digit of the 2-digit number *3, it turns out that six
 - of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
 -
 - By replacing the 3rd and 4th digits of 56**3 with the same digit, this
 - 5-digit number is the first example having seven primes among the ten
 - generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
 - 56773, and 56993. Consequently 56003, being the first member of this family,
 - is the smallest prime with this property.
 -
 - Find the smallest prime which, by replacing part of the number (not
 - necessarily adjacent digits) with the same digit, is part of an eight prime
 - value family.
 -}
{-
 - The "do not use leading zeros in the mask" restriction is not stated, but
 - it is important. This is manifested in the primeFamily function.
 -}
{-
 - I made a small optimization: the is_prime' array, which speeds up the
 - prime-testing. This takes it from ~4.3s to ~1.15s
 -
 - I'm pretty sure that about 0.6s are spent just generating the (core,mask)
 - pairs, so that would be another area to check.
 -}
import qualified Util.Prime as Prime

solve :: String
solve = show solveProblem

isPrime n = n >= 2 && sieve ! n
  where
    sieve = Prime.sieve 1e6

-- Given a core and a mask, generates all the primes in that family
-- Ex: primeFamily 20 1 -> [23, 29] since those are the only primes of the form 2*
primeFamily core mask =
  let family =
        [ core + k * mask
        | k <-
            if mask < core
              then [0 .. 9]
              else [1 .. 9]
        ]
  in filter isPrime family

-- Pads a list to size `s`. Does so by padding on the left with zeros.
padTo s xs = replicate (s - length xs) 0 ++ xs

-- all the core/mask pairs of size s
-- Ex. coreMaskPairs 5 would include [12030, 101] and [55550, 1]
coreMaskPairs s =
  let raw_cms = coreMaskPairs' s
      with_mask = filter (\(c, m) -> m > 0) raw_cms
  in with_mask

coreMaskPairs' 0 = [(0, 0)]
coreMaskPairs' s =
  let cm_pairs = coreMaskPairs' (s - 1)
  in concat
       [ (10 * c, 10 * m + 1) : [(10 * c + i, 10 * m) | i <- [0 .. 9]]
       | (c, m) <- cm_pairs
       ]

families =
  [primeFamily core mask | len <- [1 ..], (core, mask) <- coreMaskPairs len]

findPrimeFamilies s = filter (\f -> length f == s) families

solveProblem =
  let ans = findPrimeFamilies 8
  in minimum $ head ans
