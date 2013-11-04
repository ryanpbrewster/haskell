-- bit_positions.hs
{-
 - Challenge Description:
 -
 - Given a number n and two integers p1,p2 determine if the bits in position p1 and p2 are the same or not. Positions p1 and p2 and 1 based.
 - Input sample:
 -
 - The first argument will be a text file containing a comma separated list of 3 integers, one list per line. E.g.
 -
 - 86,2,3
 - 125,1,2
 -
 - Output sample:
 -
 - Print to stdout, 'true'(lowercase) if the bits are the same, else 'false'(lowercase). E.g.
 -
 - true
 - false
 -}

import Data.Bits
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let lns = lines txt
                       inps = [ map read $ wordsBy (==',') ln | ln <- lns ]
                       anss = [ bitsMatch n p1 p2 | [n,p1,p2] <- inps ]
                       outs = map processOutput anss
                   in unlines outs

processOutput False = "false"
processOutput True  = "true"

bitsMatch n p1 p2 = n `testBit` (p1-1) == n `testBit` (p2-1)
