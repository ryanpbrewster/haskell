-- repeated_substring.hs
{-
 - You are to find the longest repeated substring in a given text. Repeated
 - substrings may not overlap. If more than one substring is repeated with the
 - same length, print the first one you find.(starting from the beginning of
 - the text). NOTE: The substrings can't be all spaces
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename.The
 - input file contains several lines. Each line is one test case. Each line
 - contains a test string. eg.
 -
 -     banana
 -
 - Output sample:
 -
 - For each set of input produce a single line of output which is the longest
 - repeated substring. If there is none, print out the string NONE. eg.
 -
 -     an
 -}

import System.Environment (getArgs)
import Data.List (maximumBy, inits, tails, isInfixOf)
import Data.Ord (comparing)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let inputs = lines txt
                       anss = map longestRepeatedSubstring inputs
                       outputs = [ if null ans then "NONE" else ans | ans <- anss ]
                   in unlines outputs

longestRepeatedSubstring str =
    maximumBy (comparing length) $ reverse $ map longestRepeatedPrefix (tails str)

longestRepeatedPrefix str = let cans = zip (inits str) (tails str)
                                anss = filter (\(pre,rest) -> pre `isInfixOf` rest) cans
                                anss' = filter (any (/= ' ')) $ map fst anss
                            in if null anss' then "" else last anss'
