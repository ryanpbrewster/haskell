-- swap_case.hs
{-
 - Write a program which swaps letters' case in a sentence. All non-letter characters should remain the same.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Input example is the following
 -
 - Hello world!
 - JavaScript language 1.8
 - A letter
 - Output sample:
 -
 - Print results in the following way.
 -
 - hELLO WORLD!
 - jAVAsCRIPT LANGUAGE 1.8
 - a LETTER
 -}


import System.Environment (getArgs)
import Data.Char (isUpper, isLower, toUpper, toLower)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = lines txt
                       outputs = [ map swapCase ln | ln <- inputs ]
                   in unlines outputs

swapCase ch | isUpper ch = toLower ch
            | isLower ch = toUpper ch
            | otherwise  = ch
