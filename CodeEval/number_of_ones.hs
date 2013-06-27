-- number_of_ones.hs
{-
 - Write a program to determine the number of 1 bits in the internal representation of a given integer.
 - Input sample:
 -
 - The first argument will be a text file containing an integer, one per line. e.g.
 -
 - 10
 - 22
 - 56
 -
 - Output sample:
 -
 - Print to stdout, the number of ones in the binary form of each number.
 - e.g.
 -
 - 2
 - 3
 - 3
 -}


import System.Environment (getArgs)
import qualified Data.Bits as Bits

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = [ read ln :: Int | ln <- lines txt ]
                       ans = map Bits.popCount inputs
                   in unlines $ map show ans
