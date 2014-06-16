-- RNA.hs
{-
 - Problem
 -
 - An RNA string is a string formed from the alphabet containing 'A', 'C', 'G', and 'U'.
 -
 - Given a DNA string t corresponding to a coding strand, its transcribed RNA string u is formed by replacing all occurrences of 'T' in t with 'U' in u.
 -
 - Given: A DNA string t having length at most 1000 nt.
 -
 - Return: The transcribed RNA string of t.
 - Sample Dataset
 -
 - GATGGAACTTGACTACGTAAATT
 -
 - Sample Output
 -
 - GAUGGAACUUGACUACGUAAAUU
 -}

import System.Environment (getArgs)
import Data.Text (pack, unpack, replace, singleton)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let inp = pack $ concat $ lines txt
                       ans = replace (singleton 'T') (singleton 'U') inp
                   in unpack ans

