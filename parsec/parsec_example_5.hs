-- parsec_example_5.hs
{-
 - A very simple, short example of parsing a CSV file,
 - taken from Real World Haskell
 -}

import Text.ParserCombinators.Parsec

csvFile = endBy line eol -- take `line' until you find `eol'
line = sepBy cell (char ',') -- split up `cell' by `(char ',')'
cell = many (noneOf ",\n")
eol = char '\n'


parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(error!)" input
