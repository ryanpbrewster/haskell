-- parsec_example_0.hs
{-
 - Simple parser example simply parses a list like
 -     a,a,a,a,a,a,a,a
 - into a list of characters
 -     ['a', 'a', ...] == "aaaaa..."
 -}

import Text.ParserCombinators.Parsec

line :: GenParser Char st [String]
line = do
    result <- cells
    eol
    return result

eol = char '\n'

cells = do
    first <- cellContent
    next <- remainingCells
    return (first:next)

remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)
    <|> (return [])

cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

parseLine input = parse line "(unknown)" input
