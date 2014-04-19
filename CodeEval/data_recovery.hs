{-
 - Your friends decided to make a fun of you. They've installed a script to
 - your computer which shuffled all of the words within a text. It was a joke,
 - so they've left hints for each sentence which allow you to easily rebuild
 - your data. The challenge is to write a program which reconstructs each
 - sentence out of a set of words, you need to find out how to use a given hint
 - and print out the original sentences.
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line is a test case. Each test case consists of a set of words and
 - a sequence of numbers separated by a semicolon. The words within a set and
 - the numbers within a sequence are separated by a single whitespace. E.g.
 - 1
 - 2
 - 3
 - 2000 and was not However, implemented 1998 it until;9 8 3 4 1 5 7 2
 - programming first The language;3 2 1
 - programs Manchester The written ran Mark 1952 1 in Autocode from;6 2 1 7 5 3 11 4 8 9
 - Output sample:
 -
 - For each test case print out the reconstructed sentence one per line. E.g.
 - 1
 - 2
 - 3
 - However, it was not implemented until 1998 and 2000
 - The first programming language
 - The Manchester Mark 1 ran programs written in Autocode from 1952
 -
 - Constraints:
 - The number of test cases is in range [20, 40].
 - The words consist of ASCII upper and lower case letters, digits and punctuation. 
 -}

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Data.List (sort)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let lns = lines txt
                       inps = map parseLine lns
                       anss = map reconstructSentence inps
                   in unlines anss

reconstructSentence (ws, hints) =
    let n = length ws
        last_hint = n*(n+1) `div` 2 - sum hints
        hints' = hints ++ [last_hint]
        ws' = zip hints' ws
    in unwords $ map snd $ sort ws'

{- Input-parsing nonsense -}
pLine = do
    sentence <- many (noneOf ";")
    char ';'
    hints <- sepBy (many digit) (char ' ')
    return (words sentence, map read hints)

parseLine :: String -> ([String], [Int])
parseLine input = case parse pLine "" input of
    Left _ -> error $ "Improperly formatted line: " ++ input
    Right v -> v
