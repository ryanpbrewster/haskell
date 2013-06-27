-- string_searching.hs
{-
 - You are given two strings. Determine if the second string is a substring of the first (Do NOT use any substr type library function). The second string may contain an asterisk(*) which should be treated as a regular expression i.e. matches zero or more characters. The asterisk can be escaped by a \ char in which case it should be interpreted as a regular '*' character. To summarize: the strings can contain alphabets, numbers, * and \ characters.
 - Input sample:
 -
 - File containing two comma delimited strings per line. e.g.
 -
 - Hello,ell
 - This is good, is 
 - CodeEval,C*Eval
 - Old,Young
 -
 - Output sample:
 -
 - If the second string is indeed a substring of the first, print out a 'true'(lowercase), else print out a 'false'(lowercase), one per line.
 - e.g.
 -
 - true
 - true
 - true
 - false
 -}

{-
 - Note that this is much more naturally expressed as a "stringS searching" problem.
 - Each * breaks the substring into two substrings. For instance, if we were looking for
 - "e*lo" in "Hello World", we would really need to search for "e" and "lo", in
 - that order, in "Hello World"
 -}

import System.Environment (getArgs)
import Data.List (isPrefixOf)

-- see if <a> contains all the substrings in <ss>, in order
containsSubstrings a [] = True
containsSubstrings [] _ = False
containsSubstrings a ss@(f:r) | f `isPrefixOf` a = containsSubstrings (drop (length f) a) r
                              | otherwise        = containsSubstrings (tail a) ss

-- this is gross and I hate it, but it seems to work
splitIntoSubstrings [] = [""]
splitIntoSubstrings s | "\\*" `isPrefixOf` s = let ss = splitIntoSubstrings (drop 2 s)
                                             in ("\\*" ++ head ss):(tail ss)
                      | "*" `isPrefixOf` s  = "" : splitIntoSubstrings (tail s)
                      | otherwise           = let ss = splitIntoSubstrings (drop 1 s)
                                              in (head s : head ss):(tail ss)

solveProblem txt = let inputs = [ (l,tail r) | ln <- lines txt, let (l,r) = break (==',') ln ]
                       ans = [ containsSubstrings l (splitIntoSubstrings r) | (l,r) <- inputs ]
                       outputs = [ if x then "true" else "false" | x <- ans ]
                   in unlines outputs

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt
