-- parsec_example_6.hs
{-
 - Try to parse
 -     int: (double,double); (double,double); ...; (double,double)
 -
 -}

import Text.ParserCombinators.Parsec

pLine = do
    idx <- pIndex
    spaces
    char ':'
    pts <- pPoints
    return (idx, pts)

pIndex = many (noneOf " :")
pPoints = sepBy pPoint (char ';')
pPoint = do
    spaces
    char '('
    x <- many (noneOf ",")
    char ','
    y <- many (noneOf ")")
    char ')'
    spaces
    return (x,y)


parseLine :: String -> Either ParseError (String, [(String,String)])
parseLine input = parse pLine "(error!)" input
