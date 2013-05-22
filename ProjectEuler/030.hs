-- 030.hs
{-
 - Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
 -
 -     1634 = 1^4 + 6^4 + 3^4 + 4^4
 -     8208 = 8^4 + 2^4 + 0^4 + 8^4
 -     9474 = 9^4 + 4^4 + 7^4 + 4^4
 -
 - As 1 = 14 is not a sum it is not included.
 -
 - The sum of these numbers is 1634 + 8208 + 9474 = 19316.
 -
 - Find the sum of all the numbers that can be written as the sum of fifth
 - powers of their digits.
 -}

{-
 - The first thing to note is that no number over 999999 can be written as the
 - sum of fifth powers of their digits. Any 7-digit number will necessarily map
 - down to at most a 6-digit number. Thus, we need only check numbers <= 10^6
 -
 - This program contains the immediately obvious brute-force solution.
 -}

import ProjectEuler.Math (integerDigits)

legit n = (sum $ map (^5) $ integerDigits n) == n

solveProblem = sum $ filter legit [1..999999]

main = print solveProblem
