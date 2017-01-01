module Problems.P108 (solve) where

{-
 - In the following equation x, y, and z are positive integers.
 -     1/x + 1/y == 1/z
 - For z = 4 there are exactly three distinct solutions:
 -     1/5 + 1/20 = 1/4
 -     1/6 + 1/12 = 1/4
 -     1/8 + 1/8  = 1/4
 -
 - What is the least value of z for which the number of distinct solutions
 - exceeds one-thousand?
 -}

{-
 - Since we know x > z and y > z, we write x = z+a, y = z+b which yields
 -     1/(z+a) + 1/(z+b) == 1/z
 - --> (2z+a+b)/(z+a)(z+b) == 1/z
 - --> (z+a)(z+b) == z*(2z+a+b)
 - --> z^2 + z(a+b) + ab == 2z^2 + z(a+b)
 - --> ab == z^2
 - so any two numbers (a,b) that multiply together to form z^2 will yield
 - a solution in the form of
 -     1/(z+a) + 1/(z+b) == 1/z
 - Thus, we compute the divisors of z and pluck out the (a,b) pairs. For instance,
 - z = 5 yields Divisors[5^2] == {1,5,25}
 - --> (a,b) = { (1,25), (5,5) }
 - --> (x,y) = { (6,30), (10,10) }
 -}

{-
 - We are looking for a number with a lot of very small divisors.
 - Thus, it makes NO sense to look at numbers like 19, since 19 has the same
 - number of divisors as 2, but is much larger.
 -
 - What I do here is look systematically at numbers with a given number of prime factors.
 - First: 0 factors --> [1]
 -        1 factor  --> [2]
 -        2 factors --> [4, 6]
 -        3 factors --> [8, 12, 30]
 -        4 factors --> [2^4, 2^3 3, 2^2 3^2, 2^2 3 5, 2 3 5 7]
 - Essentially what we're doing is looking at the integer partitions and
 - using them as prime exponents. Thus,
 -     partitions 1 = { [1] } --> {2}
 -     partitions 2 = { [2], [1,1] } --> {2^2, 2 3}
 -                    { [3], [2,1], [1,1,1] } --> {2^3, 2^2 3, 2 3 5}
 -
 - If we do it this way, we'll avoid looking at a LOT of silly numbers.
 -}

import Data.List (sort)

import Util.Prime (sigma, primes)
import Util.Math (fullCoinCombos)
import Util.List (mergeInf)

solve :: String
solve = show $ solveProblem (10^3)

parts = fullCoinCombos [1..]

expsToInt :: Integral a => [a] -> Integer
expsToInt = product . zipWith (^) primes

ndivs = sigma 0
numSolutions z = 1 + (ndivs (z^2)) `div` 2

solveProblem bound =
    let candidates = mergeInf $ [ sort $ map expsToInt ps | ps <- parts ]
    in head $ filter (\z -> numSolutions z > bound) candidates
