-- complementing_a_strand_of_dna.hs
{-
 - Problem
 -
 - In DNA strings, symbols 'A' and 'T' are complements of each other, as are
 - 'C' and 'G'.
 -
 - The reverse complement of a DNA string s is the string sc formed by
 - reversing the symbols of s, then taking the complement of each symbol (e.g.,
 - the reverse complement of "GTCA" is "TGAC").
 -
 - Given: A DNA string s of length at most 1000 bp.
 -
 - Return: The reverse complement sc of s.
 - Sample Dataset
 -
 - AAAACCCGGT
 -
 - Sample Output
 -
 - ACCGGGTTTT
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let inp = concat $ lines txt
                       ans = reverseComplement inp
                   in ans

reverseComplement dna = map complement $ reverse dna

complement 'A' = 'T'
complement 'T' = 'A'
complement 'C' = 'G'
complement 'G' = 'C'
