-- DNA.hs
{-
 - Problem
 -
 - A string is simply an ordered collection of symbols selected from some
 - alphabet and formed into a word; the length of a string is the number of
 - symbols that it contains.
 -
 - An example of a length 21 DNA string (whose alphabet contains the symbols
 - 'A', 'C', 'G', and 'T') is "ATGCTTCAGAAAGGTCTTACG."
 -
 - Given: A DNA string s of length at most 1000 nt.
 -
 - Return: Four integers (separated by spaces) counting the respective number
 - of times that the symbols 'A', 'C', 'G', and 'T' occur in s.
 -
 - Sample Dataset
 -
 - AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
 -
 - Sample Output
 -
 - 20 12 17 21
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

alphabet = "ACGT"

count x [] = 0
count x (y:ys) = (if x == y then 1 else 0) + count x ys

solveProblem txt = let inp = concat $ lines txt
                       ans = [ count ch inp | ch <- alphabet ]
                   in unwords $ map show ans
