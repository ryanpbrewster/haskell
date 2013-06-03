-- 033.hs
{-
 - The fraction 49/98 is a curious fraction, as an inexperienced mathematician
 - in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
 - is correct, is obtained by cancelling the 9s.
 -
 - We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
 -
 - There are exactly four non-trivial examples of this type of fraction, less
 - than one in value, and containing two digits in the numerator and
 - denominator.
 -
 - If the product of these four fractions is given in its lowest common terms,
 - find the value of the denominator.
 -}

import Data.Ratio

main = print solveProblem

solveProblem = let examples = nontrivialExamples
                   reduced = [ n%d | [n,d] <- examples ]
               in denominator $ product reduced


nontrivialExamples = [ [n,d] | p <- [1..9]
                             , q <- [1..9]
                             , r <- [1..9]
                             , s <- [1..9]
                             , let n = round (10*p + q)
                             , let d = round (10*r + s)
                             , n < d
                             , isCurious p q r s ]


isCurious a b c d | a == c && (10*a + b) / (10*c + d) == b / d = True
                  | a == d && (10*a + b) / (10*c + d) == b / c = True
                  | b == c && (10*a + b) / (10*c + d) == a / d = True
                  | b == d && (10*a + b) / (10*c + d) == a / c = True
                  | otherwise                                  = False

