-- Project Euler Problem 23
-- An abundant number is one where DivisorSigma[1, n] == 2*n
-- For instance, 28 is an abundant number, since
--     DivisorSigma[1, 28] = 1 + 2 + 4 + 7 + 14 + 28 == 2*28
-- It is helpful to think of proper divisors (divisors of n that are < n)
--
-- It is known that all numbers > 28123 can be written as the sum of two
-- abundant numbers. Find the sum of all numbers <= 28123 that CANNOT
-- be written in such a way.

import qualified ProjectEuler.Prime as Prime
import Data.List(nub)
import Data.Array -- not my favorite way to import things, but it is convenient


bound = 28123
abundants = [ x | x <- [2..bound], Prime.sigma 1 x > 2*x ]

process bl [] = bl
process bl (x:xs) = let bl' = bl // [(x+y,True) | y <- takeWhile (\z -> x+z <= bound) (x:xs)]
                    in process bl' xs

asum arr = sum [ arr!i | i <- [lo..hi] ]
    where (lo,hi) = bounds arr

solveProblem = let bl = array (0,bound) [(i,False) | i <- [0..bound]]
                   can_make = process bl abundants
                   cant_make = [ i | i <- [0..bound], not (can_make!i) ]
               in sum cant_make

main = print solveProblem
