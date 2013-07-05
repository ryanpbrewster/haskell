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

{-
 - The problem statement is incomplete. It is NOT guaranteed that every Fi will
 - be possible to replace. If you find an Fi that is not part of the string,
 - simply ignore it.
 -}

import Data.List (isPrefixOf, isInfixOf)

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

solveProblem txt = let inputs = [(str, subs) | ln <- lines txt
                                             , let wrds = wordsBy (`elem` ",;") ln
                                             , let str = head wrds
                                             , let subs = pairs (tail wrds) ]
                       outputs = [ processString str subs | (str,subs) <- inputs ]
                   in unlines outputs


data StrChunk = SC { getStr :: String
                   , getVis :: Bool
                   }
instance Show StrChunk where
    show sc = showStrChunk sc
showStrChunk (SC s v) = show (s,v)

concatChunks :: [StrChunk] -> String
concatChunks = concat . map getStr

processString s subs = substituteStrings [SC s False] subs

-- pieces of the string to be replaced
--                       |        substitution pairs
--                       |                |
--                       v                v
substituteStrings :: [StrChunk] -> [(String,String)] -> String
substituteStrings chks [] = concatChunks chks
substituteStrings chks (sub:subs) = substituteStrings (makeOneSub chks sub) subs

makeOneSub :: [StrChunk] -> (String,String) -> [StrChunk]
makeOneSub [] _ = []
makeOneSub (chk:chks) (si, sf)
    | getVis chk || not (si `isInfixOf` (getStr chk)) = chk : makeOneSub chks (si,sf)
    | otherwise = let s = getStr chk
                      (l,_,r) = breakIntoPieces s si
                      rest = makeOneSub ((SC r False):chks) (si,sf)
                  in (SC l False):(SC sf True):rest

-- breakIntoPieces takes a string, and a substring, and breaks it
-- into the portion before ss, ss, and the portion after ss
-- Should obey: let (l,_,r) = breakIntoPieces str ss in (l ++ ss ++ r) == str
breakIntoPieces str@(ch:rest) ss | ss `isPrefixOf` str = ("", ss, drop (length ss) str)
                                 | otherwise           =
    let (l,_,r) = breakIntoPieces rest ss
    in (ch:l, ss, r)



