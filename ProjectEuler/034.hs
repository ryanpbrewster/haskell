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

factorial 0 = 1
factorial n = n * factorial (n-1)

legit n = (sum $ map factorial $ integerDigits n) == n

solveProblem = sum $ filter legit [1..10^6-1]

main = print solveProblem
