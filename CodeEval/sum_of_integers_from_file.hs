-- sum_of_integers_from_file.hs
{-
 - Print out the sum of integers read from a file.
 - Input sample:
 -
 - The first argument to the program will be a text file containing a positive integer, one per line. e.g.
 -
 - 5
 - 12
 -
 - NOTE: For solutions in JavaScript, assume that there are 7 lines of input
 -
 - Output sample:
 -
 - Print out the sum of all the integers read from the file.
 - e.g.
 -
 - 17
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = map read $ lines txt
                   in show $ sum inps
