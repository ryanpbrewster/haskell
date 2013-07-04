-- unique_elements.hs
{-
 - You are given a sorted list of numbers with duplicates. Print out the sorted
 - list with duplicates removed.
 -
 - Input sample:
 -
 - File containing a list of sorted integers, comma delimited, one per line. e.g. 
 -
 - 1,1,1,2,2,3,3,4,4
 - 2,3,4,5,5
 - Output sample:
 -
 - Print out the sorted list with duplicates removed, one per line
 - e.g.
 -
 - 1,2,3,4
 - 2,3,4,5
 -}

import System.Environment (getArgs)
import Data.List (group, intercalate)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let inputs = [ wordsBy (==',') ln | ln <- lines txt ]
                       anss = [ map head $ group items | items <- inputs ]
                       outputs = [ intercalate "," uniqs | uniqs <- anss ]
                   in unlines outputs

