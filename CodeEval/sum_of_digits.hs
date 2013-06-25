-- sum_of_digits.hs
{-
 - Given a positive integer, find the sum of its constituent digits.
 - Input sample:
 -
 - The first argument will be a text file containing positive integers, one per line. e.g.
 -
 - 23
 - 496
 -
 - Output sample:
 -
 - Print to stdout, the sum of the numbers that make up the integer, one per line.
 - e.g.
 -
 - 5
 - 19
 -}

import System.Environment (getArgs)

sumOfDigits 0 = 0
sumOfDigits n = let (q,r) = n `divMod` 10
                in r + sumOfDigits q

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = map read $ lines txt
                       outputs = map sumOfDigits inps
                   in unlines $ map show outputs
