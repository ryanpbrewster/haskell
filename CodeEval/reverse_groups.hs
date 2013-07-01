-- reverse_groups.hs
{-
 - Given a list of numbers and a positive integer k, reverse the elements of
 - the list, k items at a time. If the number of elements is not a multiple of
 - k, then the remaining items in the end should be left as is.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains a list of numbers and the number k, separated by
 - a semicolon. The list of numbers are comma delimited. e.g.
 -
 - 1,2,3,4,5;2
 - 1,2,3,4,5;3
 -
 - Output sample:
 -
 - Print out the new comma separated list of numbers obtained after reversing. e.g.
 -
 - 2,1,4,3,5
 - 3,2,1,4,5
 -}

import System.Environment (getArgs)
import Data.List (intercalate)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let lns = [ wordsBy ",;" ln | ln <- lines txt ]
                       inputs = [ (read $ last ln, init ln) | ln <- lns ]
                       anss = [ reverseChunks k xs | (k,xs) <- inputs ]
                       outputs = [ intercalate "," ans | ans <- anss ]
                   in unlines outputs

reverseChunks k xs =
    concat [ if length ck == k then reverse ck else ck | ck <- chunksOf k xs ]

chunksOf _ [] = []
chunksOf k xs = let (f,r) = splitAt k xs in f:(chunksOf k r)

wordsBy delims s = wordsBy' delims s
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)
