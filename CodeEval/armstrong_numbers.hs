-- armstrong_numbers.hs
{-
 - An Armstrong number is an n-digit number that is equal to the sum of the
 - n'th powers of its digits. Determine if the input numbers are Armstrong
 - numbers.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file has a positive integer. e.g.
 -
 - 6
 - 153
 - 351
 -
 - Output sample:
 -
 - Print out True/False if the number is an Armstrong number or not e.g.
 -
 - True
 - True
 - False
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = map read $ lines txt
                       anss = map isArmstrong inputs
                   in unlines $ map show anss

isArmstrong x = let digits = integerDigits x
                    n = length digits
                in sum [ d^n | d <- digits ] == x

integerDigits = reverse . integerDigits'
    where integerDigits' 0 = []
          integerDigits' n = let (q,r) = n `quotRem` 10 in r : integerDigits' q
