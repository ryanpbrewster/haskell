-- 145.hs
-- HOLY COW SLOW (17m 37s)
{-
 - Some positive integers n have the property that the sum [n + reverse(n)]
 - consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and
 - 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and
 - 904 are reversible. Leading zeroes are not allowed in either n or
 - reverse(n).
 -
 - There are 120 reversible numbers below one-thousand.
 -
 - How many reversible numbers are there below one-billion (109)?
 -}

import ProjectEuler.Math (fromIntegerDigits, integerDigits)

intReverse n = intReverse_h n 0
intReverse_h 0 n' = n'
intReverse_h n n' = let (q,r) = n `divMod` 10
                    in intReverse_h q (10*n' + r)

isReversible n | n `mod` 10 == 0 = False
               | otherwise       = let s = n + (intReverse n)
                                   in all odd $ integerDigits s

solveProblem bound = length $ filter isReversible [1..bound]

main = print $ solveProblem (10^9)
