-- string_substitution.hs
{-
 - Credits: This challenge was contributed by Sam McCoy
 -
 - Given a string S, and a list of strings of positive length,
 - F1,R1,F2,R2,...,FN,RN, proceed to find in order the occurrences
 - (left-to-right) of Fi in S and replace them with Ri. All strings are over
 - alphabet { 0, 1 }. Searching should consider only contiguous pieces of
 - S that have not been subject to replacements on prior iterations. An
 - iteration of the algorithm should not write over any previous replacement by
 - the algorithm.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file is one test case. Each test case will contain a string,
 - then a semicolon and then a list of comma separated strings.eg.
 -
 - 10011011001;0110,1001,1001,0,10,11
 -
 - Output sample:
 -
 - For each line of input, print out the string after substitutions have been made.eg.
 -
 - 11100110
 -
 - For the curious, here are the transitions for the above example:
 -        10011011001
 -     -> 10100111001 [replacing 0110 with 1001]
 -     -> 10100110    [replacing 1001 with 0]
 -     -> 11100110    [replacing 10 with 11]
 -}

import Data.List (isPrefixOf)
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt 

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

pairs [] = []
pairs (x:x':xs) = (x,x') : pairs xs

solveProblem txt =
    let inputs = [(str, subs) | ln <- lines txt
                              , let (str:rest) = wordsBy (`elem` ",;") ln
                              , let subs = pairs rest ]
        outputs = [ stringSubs str subs | (str,subs) <- inputs ]
    in unlines outputs

data StrChunk = SC { getStr :: String, getVis :: Bool }

stringSubs str subs = let unit = [SC str False]
                      in concat $ map getStr $ foldl stringSub unit subs

stringSub chks (si,sf) = concat $ map (replace si sf) chks

replace si sf chk | getVis chk = [chk]
                  | otherwise =
    let pieces = breakAroundSubstring (getStr chk) si
    in [if p == si then SC sf True else SC p False | p <- pieces ]

breakAroundSubstring str ss = filter (not.null) $ breakAroundSubstring' str ss
breakAroundSubstring' "" _ = [""]
breakAroundSubstring' str@(ch:rest) ss
    | ss `isPrefixOf` str = "" : ss : breakAroundSubstring' (drop (length ss) str) ss
    | otherwise = let (f:ps) = breakAroundSubstring' rest ss in (ch:f):ps
