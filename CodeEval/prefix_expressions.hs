-- prefix_expressions
{-
 - You are given a prefix expression. Write a program to evaluate it.
 - Input sample:
 -
 - The first argument will be an input file with one prefix expression per
 - line. e.g.
 -
 - * + 2 3 4
 -
 - Your program has to read this and insert it into any data structure you
 - like. Traverse that data structure and evaluate the prefix expression. Each
 - token is delimited by a whitespace. You may assume that the only valid
 - operators appearing in test data are '+','*' and '/'
 -
 - Output sample:
 -
 - Print to stdout, the output of the prefix expression, one per line. e.g.
 -
 - 20
 -}

import System.Environment (getArgs)

-- if there is only one thing left in the expression, it must be a number
evaluateExpression [] = []
evaluateExpression (x:xs) | x == "*" = let (a:b:r) = evaluateExpression xs in (a*b):r
                          | x == "+" = let (a:b:r) = evaluateExpression xs in (a+b):r
                          | x == "/" = let (a:b:r) = evaluateExpression xs in (a `quot` b):r
                          | otherwise = (read x):(evaluateExpression xs)

solveProblem txt = let inputs = [words ln | ln <- lines txt]
                       outputs = map (head.evaluateExpression) inputs
                   in unlines $ map show outputs

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt
