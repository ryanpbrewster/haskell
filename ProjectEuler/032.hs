-- 032.hs
{-
 - We shall say that an n-digit number is pandigital if it makes use of all the
 - digits 1 to n exactly once; for example, the 5-digit number, 15234, is
 - 1 through 5 pandigital.
 -
 - The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
 - multiplicand, multiplier, and product is 1 through 9 pandigital.
 -
 - Find the sum of all products whose multiplicand/multiplier/product identity
 - can be written as a 1 through 9 pandigital.  HINT: Some products can be
 - obtained in more than one way so be sure to only include it once in your
 - sum.
 -}

import ProjectEuler.Math (integerDigits)
import Data.List (sort, nub)

main = print solveProblem

third (_,_,c) = c

uniqueDigits n = let digits = integerDigits n
                 in nub digits == digits

solveProblem = sum $ nub $ map third $ [ (a,b,a*b) | a <- filter uniqueDigits [1..99]
                                                   , b <- filter uniqueDigits [100..9999]
                                                   , isPandigitalProduct a b ]

isPandigitalProduct a b =
    let digits = (integerDigits a) ++ (integerDigits b) ++ (integerDigits (a*b))
    in sort digits == [1..9]
