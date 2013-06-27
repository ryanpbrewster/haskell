-- trailing_string.hs
{-
 - You are given two strings 'A' and 'B'. Print out a 1 if string 'B' occurs at the end of string 'A'. Else a zero.
 - Input sample:
 -
 - The first argument is a file, containing two strings, comma delimited, one per line. Ignore all empty lines in the input file.e.g.
 -
 - Hello World,World
 - Hello CodeEval,CodeEval
 - San Francisco,San Jose
 -
 - Output sample:
 -
 - Print out 1 if the second string occurs at the end of the first string. Else zero. Do NOT print out empty lines between your output.
 - e.g.
 -
 - 1
 - 1
 - 0
 -}


import System.Environment (getArgs)
import Data.List (isSuffixOf)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = [ wordsBy "," ln | ln <- lines txt ]
                       outputs = [ if b `isSuffixOf` a then "1" else "0" | [a,b] <- inputs ]
                   in unlines outputs

wordsBy delims s = wordsBy' delims s
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)
