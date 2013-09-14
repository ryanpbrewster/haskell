-- longest_word.hs
{-
 - In this challenge you need to find the longest word in a sentence. If the sentence has more than one word of the same length you should pick the first one.
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Input example is the following
 -
 - some line with text
 - another line
 -
 - Each line has one or more words. Each word is separated by space char.
 - Output sample:
 -
 - Print the longest word in the following way.
 -
 - some
 - another
 -}


import System.Environment (getArgs)
import Data.List (maximumBy)
import Data.Ord (comparing)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let lns = lines txt
                       anss = map longestWord lns
                   in unlines anss


longestWord sen = maximumBy (comparing length) (reverse $ words sen)
