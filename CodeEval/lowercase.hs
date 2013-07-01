-- lowercase.hs
{-
 - Given a string write a program to convert it into lowercase.
 - Input sample:
 -
 - The first argument will be a text file containing sentences, one per line.
 - You can assume all characters are from the english language. e.g.
 -
 - HELLO CODEEVAL
 - This is some text
 -
 - Output sample:
 -
 - Print to stdout, the lowercase version of the sentence, each on a new line.
 - e.g.
 -
 - hello codeeval
 - this is some text
 -}


import System.Environment (getArgs)
import Data.Char (toLower)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = map toLower txt
