module Problems.P204 (solve) where

{-
 - A Hamming number is a positive number which has no prime factor larger than
 - 5.  So the first few Hamming numbers are 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15.
 - There are 1105 Hamming numbers not exceeding 10^8.
 -
 - We will call a positive number a generalised Hamming number of type n, if it
 - has no prime factor larger than n.  Hence the Hamming numbers are the
 - generalised Hamming numbers of type 5.
 -
 - How many generalised Hamming numbers of type 100 are there which don't
 - exceed 10^9?
 -}

import qualified Util.Prime as Prime
import qualified Data.Set as DS

hammingNumbers n bound = let ps = takeWhile (<= n) Prime.primes
                         in allMultiples ps bound

allMultiples [] _ = [1]
allMultiples (p:ps) bound = let xs = allMultiples ps bound
                                p_powers = takeWhile (<=bound) $ iterate (p*) p
                                xs_with_p = [ p' * x | p' <- p_powers, x <- xs ]
                            in xs ++ (filter (<= bound) xs_with_p)

solveProblem n bound = length $ hammingNumbers n bound

solve = show $ solveProblem 100 (10^9)
