-- detecting_cycles.hs
{-
 - Given a sequence, write a program to detect cycles within it.
 - Input sample:
 -
 - A file containing a sequence of numbers (space delimited). The file can have multiple such lines. e.g
 -
 - 2 0 6 3 1 6 3 1 6 3 1
 -
 - Ensure to account for numbers that have more than one digit eg. 12. If there is no sequence, ignore that line.
 -
 - Output sample:
 -
 - Print to stdout the first sequence you find in each line. Ensure that there are no trailing empty spaces on each line you print. e.g.
 -
 - 6 3 1
 -}

{-
 - The definition of "cycles" is left unclear. I assume it means that you have
 - to find the first occurrence of a repeated, adjacent, non-empty sequence. So
 - a0 a1 a2 ... {a[i],...,a[i+m]} {a[i]...a[i+m]}
 -}

import Data.List (isPrefixOf, inits, tails)
import Data.Maybe
import System.Environment (getArgs)


findRepeatedSequence xs =
    let srs = tail $ zip (inits xs) (tails xs)
        anss = [ s | (s,r) <- srs, s `isPrefixOf` r ]
    in if null anss then Nothing else Just (head anss)

findCycle xs = let ends = map reverse $ inits xs
                   anss = map reverse $ mapMaybe findRepeatedSequence ends
               in if null anss then Nothing else Just (head anss)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let inputs = [ map read $ words ln | ln <- lines txt ]
                       anss = mapMaybe findCycle inputs :: [[Int]]
                       outputs = [ unwords $ map show ans | ans <- anss ]
                   in unlines outputs
