module Problems.P087
  ( solve
  ) where

import qualified Data.Set as DS
{-
 - The smallest number expressible as the sum of a prime square, prime cube,
 - and prime fourth power is 28. In fact, there are exactly four numbers below
 - fifty that can be expressed in such a way:
 -
 - 28 = 2^2 + 2^3 + 2^4
 - 33 = 3^2 + 2^3 + 2^4
 - 49 = 5^2 + 2^3 + 2^4
 - 47 = 2^2 + 3^3 + 2^4
 -
 - How many numbers below fifty million can be expressed as the sum of a prime
 - square, prime cube, and prime fourth power?
 -}
import qualified Util.Prime as Prime

solve :: String
solve = show solveProblem

solveProblem = generalProblem (50 * 10 ^ 6)

generalProblem bound =
  let can_make =
        [ p2 + p3 + p4
        | p2 <- takeWhile (< bound) $ map (^ 2) Prime.primes
        , p3 <- takeWhile (< bound - p2) $ map (^ 3) Prime.primes
        , p4 <- takeWhile (< bound - p2 - p3) $ map (^ 4) Prime.primes
        ]
  in DS.size $ DS.fromList can_make
