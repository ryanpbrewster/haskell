-- multiples_of_a_number.hs
{-
 - Given numbers x and n, where n is a power of 2, print out the smallest
 - multiple of n which is greater than or equal to x. Do not use division or
 - modulo operator.
 -
 - Input sample:
 - The first argument will be a text file containing a comma separated list
 - of two integers, one list per line. e.g.
 -
 - 13,8
 - 17,16
 -
 - Output sample:
 - Print to stdout, the smallest multiple of n which is greater than or equal
 - to x, one per line.
 - e.g.
 -
 - 16
 - 32
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let xns = [ map read $ wordsBy "," ln | ln <- lines txt ]
                       outputs = [ nextMultiple n x | [x,n] <- xns ]
                   in unlines $ map show outputs

nextMultiple n x = n + n*(x `div` n)

wordsBy delims s = wordsBy' delims s
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)
