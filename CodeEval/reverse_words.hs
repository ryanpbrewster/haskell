-- reverse_words.hs
{-
 - Write a program to reverse the words of an input sentence.
 - Input sample:
 -
 - The first argument will be a text file containing multiple sentences, one
 - per line. Possibly empty lines too. e.g.
 -
 - Hello World
 - Hello CodeEval
 -
 - Output sample:
 -
 - Print to stdout, each line with its words reversed, one per line.
 - Empty lines in the input should be ignored. Ensure that there are no
 - trailing empty spaces on each line you print.
 - e.g.
 -
 - World Hello
 - CodeEval Hello
 -}

import System.Environment (getArgs)

solveProblem txt = unlines [ unwords $ reverse $ words ln | ln <- lines txt ]

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt
