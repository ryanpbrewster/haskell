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

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = lines txt
                       anss = map valid inputs
                   in unlines $ map show anss

lefts = "({["
right '(' = ')'
right '{' = '}'
right '[' = ']'

valid inp = valid' inp ""
    where valid' "" stk = null stk
          valid' (ch:inp) stk
            | ch `elem` lefts         = valid' inp (ch:stk)
            | null stk                = False
            | ch == right (head stk)  = valid' inp (tail stk)
            | otherwise               = False
