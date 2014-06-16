-- HAMM.hs
{-
 - Problem
 -
 - Given two strings s and t of equal length, the Hamming distance between
 - s and t, denoted dH(s,t), is the number of corresponding symbols that differ
 - in s and t. See Figure 2.
 -
 - Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
 -
 - Return: The Hamming distance dH(s,t).
 -
 - Sample Dataset
 -     GAGCCTACTAACGGGAT
 -     CATCGTAATGACGGCCT
 -
 - Sample Output
 -     7
 -}


import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let [dna1, dna2] = lines txt
                       ans = hammingDistance dna1 dna2
                   in show ans

-- hammingDistance is only defined for strings of equal length
hammingDistance xx yy
    | length xx /= length yy = error "Hamming distance undefined for sequences of unequal length"
    | otherwise = sum $ [ if x == y then 0 else 1 | (x,y) <- zip xx yy ]

