-- 034.hs
{-
 - 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
 -
 - Find the sum of all numbers which are equal to the sum of the factorial of
 - their digits.
 -
 - Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 -}

-- Similar to 030.hs, observe that no 8-digit numbers can possibly satisfy
-- the criteria because they necessarily map down to <7 digit numbers.

import ProjectEuler.Math (integerDigits)
import Data.List (sort)

factorial 0 = 1
factorial n = n * factorial (n-1)

ordTuples _ 0 = [[]]
ordTuples xs k = let tups = ordTuples xs (k-1)
                 in [ x:t | t <- tups, x <- xs, null t || x <= head t ]

legit tup = let ans = sum $ map factorial tup
            in tup == (sort $ integerDigits ans)

solveProblem = let sols = filter legit $ concat [ ordTuples [0..9] k | k <- [2..7] ]
               in sum $ [ sum $ map factorial sol | sol <- sols ]

main = print solveProblem
