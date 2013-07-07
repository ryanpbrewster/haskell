-- flavius_josephus.hs
{-
 - Flavius Josephus was a famous Jewish historian of the first century, at the
 - time of the destruction of the Second Temple. According to legend, during
 - the Jewish-Roman war he was trapped in a cave with a group of soldiers
 - surrounded by Romans. Preferring death to capture, the Jews decided to form
 - a circle and, proceeding around it, to kill every j'th person remaining
 - until no one was left. Josephus found the safe spot in the circle and thus
 - stayed alive.Write a program that returns a list of n people, numbered from
 - 0 to n-1, in the order in which they are executed.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains two comma separated positive integers n and
 - m , where n is the number of people and every m'th person will be executed.
 - e.g.
 -
 - 10,3
 - 5,2
 -
 - Output sample:
 -
 - Print out the list of n people(space delimited) in the order in which they will be executed. e.g.
 -
 - 2 5 8 1 6 0 7 4 9 3
 - 1 3 0 4 2
 -}





import Debug.Trace
import System.Environment (getArgs)
import Data.List (delete)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let inputs = [ map read $ wordsBy (==',') ln | ln <- lines txt ]
                       anss = [ visitOrder [0..n-1] k | [n,k] <- inputs ]
                       outputs = [ unwords $ map show ans | ans <- anss ]
                   in unlines outputs

visitOrder xs k = visitOrder' xs
    where visitOrder' [x] = [x]
          visitOrder' xs  = let idx = (k-1) `mod` length xs
                                (l,(v:r)) = splitAt idx xs
                                xs' = r ++ l
                            in v : visitOrder' (r++l)
