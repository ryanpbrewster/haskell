-- valid_parentheses.hs
{-
 - Given a string comprising just of the characters (,),{,},[,] determine if it
 - is well-formed or not.
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains a string comprising of the characters mentioned
 - above. e.g.
 -
 - ()
 - ([)]
 -
 - Output sample:
 -
 - Print out True or False if the string is well-formed e.g.
 -
 - True
 - False
 -}

{-
 - I use a simple recursive descent parser for the language
 -     S -> VS | ""
 -     V -> (S) | [S] | {S}
 -
 -  That is, the language of all properly formed parenthesis-like strings
 -}

import System.Environment (getArgs)
import Data.Maybe

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = lines txt
                       anss = map valid inputs
                   in unlines $ map show anss


data S = SC V S | Nil
data V = VC Char S Char

parseS str = let vp = parseV str
             in case vp of
                Just (v,rest) -> let (s,fin) = parseS rest in (SC v s, fin)
                Nothing -> (Nil, str)


-- parseV returns <Just (v, rest)> if it can successfully pull a V from str
-- Otherwise, it returns Nothing
parseV (l:str) | not (l `elem` "([{") = Nothing
               | otherwise = 
    let r = rightMatch l
        (s,rest) = parseS str
    in case rest of
        (f:fin) -> if f == r then Just (VC l s r, fin) else Nothing
        _ -> Nothing
parseV _ = Nothing

rightMatch '(' = ')'
rightMatch '[' = ']'
rightMatch '{' = '}'

valid str = let (s,rest) = parseS str in rest == ""
