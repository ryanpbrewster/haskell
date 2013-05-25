-- 047_alternate.hs
{-
 - The first two consecutive numbers to have two prime factors each, with all
 - four distinct, are:
 -     14 = 2*7
 -     15 = 3*5
 -
 - The first three consecutive numbers to have three prime factors each, where
 - no adjacent number share a prime factor, are:
 -     644 = 2^2 * 7 * 23
 -     645 = 3 * 5 * 43
 -     646 = 2 * 17 * 19
 -
 - Find the first four consecutive integers to have four distinct primes
 - factors. What is the first of these numbers?
 -}

import qualified ProjectEuler.Prime as Prime
import qualified Data.Set as DS

list_factors = [ (n, DS.fromList $ Prime.factors n) | n <- [2..] ]

roll _ [] = []
roll k xs = (take k xs):(roll k $ tail xs)

kFactors k = let xss = roll k list_factors
             in [ ns | xs <- xss
                     , let ns = map fst xs
                     , let fs = map snd xs
                     , all (\e -> DS.size e == k) fs
                     , let overlaps = zipWith DS.intersection (tail fs) (init fs)
                     , all DS.null overlaps ]

solveProblem = head $ kFactors 4

main = print solveProblem
