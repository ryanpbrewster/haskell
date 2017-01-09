module Problems.P049 (solve) where

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
import Data.List (groupBy, sort, sortBy)
import Data.Ord (comparing)

solve :: String
solve = concat $ map show solveProblem

four_digit_primes = takeWhile (< 10^4) $ dropWhile (< 10^3) Prime.primes

gatherBy cmp xs = groupBy (\x -> \y -> cmp x y == EQ) $ sortBy cmp xs

pairs [] = []
pairs (x:xs) = [ [x,y] | y <- xs ] ++ pairs xs
findArithmeticSequences k xs = let x_seqs = map (makeSequence k) $ pairs xs
                               in filter (all (`elem` xs)) x_seqs

makeSequence k [x0,x1] = [ x0 + i*(x1-x0) | i <- [0..k-1] ]

solveProblem = let cmp = comparing (sort . Math.integerDigits)
                   eq_classes = gatherBy cmp four_digit_primes
                   ans = concat $ map (findArithmeticSequences 3) eq_classes
               in ans !! 1
