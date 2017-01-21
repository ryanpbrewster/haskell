module Problems.P049
  ( solve
  ) where

import Data.List (sort, tails)
import GHC.Exts (groupWith)

{-
 - The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
 - increases by 3330, is unusual in two ways:
 -     (i) each of the three terms are prime
 -     (ii) each of the 4-digit numbers are permutations of one another
 -
 - There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
 - primes, exhibiting this property, but there is one other 4-digit increasing
 - sequence.
 -
 - What 12-digit number do you form by concatenating the three terms in this
 - sequence?
 -}
import qualified Util.Math as Math
import qualified Util.Prime as Prime

solve :: String
solve = concatMap show solveProblem

fourDigitPrimes = takeWhile (< 1e4) $ dropWhile (< 1e3) Prime.primes

pairs :: [t] -> [(t, t)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

findArithmeticSequences k xs =
  let x_seqs = map (makeSequence k) $ pairs xs
  in filter (all (`elem` xs)) x_seqs

makeSequence k (x0, x1) = [x0 + i * (x1 - x0) | i <- [0 .. k - 1]]

solveProblem =
  let eq_classes = groupWith (sort . Math.integerDigits) fourDigitPrimes
      ans = concatMap (findArithmeticSequences 3) eq_classes
  in ans !! 1
