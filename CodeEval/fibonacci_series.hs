-- fibonacci_series.hs
{-
 - The Fibonacci series is defined as: F(0) = 0; F(1) = 1; F(n) = F(n-1) + F(n-2) when n>1;. Given a positive integer 'n', print out the F(n).
 - Input sample:
 -
 - The first argument will be a text file containing a positive integer, one per line. e.g.
 -
 - 5
 - 12
 -
 - Output sample:
 -
 - Print to stdout, the fibonacci number, F(n).
 - e.g.
 -
 - 5
 - 144
 -}

import System.Environment (getArgs)

fibs = [0,1] ++ zipWith (+) fibs (tail fibs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = map read $ lines txt
                       outputs = map (fibs !!) inps
                   in unlines $ map show outputs
