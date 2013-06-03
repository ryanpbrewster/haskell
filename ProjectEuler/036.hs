-- 036.hs
{-
 - The decimal number, 585 = 10010010012 (binary), is palindromic in both
 - bases.
 -
 - Find the sum of all numbers, less than one million, which are palindromic in
 - base 10 and base 2.
 -
 - (Please note that the palindromic number, in either base, may not include
 - leading zeros.)
 -}

{-
 - One small optimization: only odd numbers can be palindromic in base 2
 -}

import ProjectEuler.Math (integerDigitsBy)

main = print solveProblem

solveProblem = generalProblem (10^6)

generalProblem bound = sum $ filter legit [1,3..bound]

legit n = isPalindromic (integerDigitsBy 10 n) && isPalindromic (integerDigitsBy 2 n)

isPalindromic xs = xs == reverse xs
