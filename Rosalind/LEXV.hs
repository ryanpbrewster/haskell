-- LEXV.hs
{-
 - Given an alphabet, A = {a1, ..., am}, and a length k, form all
 - strings of length [1..k] and output them in lexicographical order
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let (alphabet, k) = parseInput txt
                       ans = tail $ preTraversal k alphabet
                   in unlines ans

parseInput txt = let [ alphabet_str, k_str ] = lines txt
                     alphabet = concat $ words alphabet_str
                     k = read k_str
                 in (alphabet, k)

preTraversal :: Int -> String -> [String]
preTraversal 0 alph = [""]
preTraversal k alph = "" : [ x : str | x <- alph, str <- preTraversal (k-1) alph ]
