-- parsec_example_7.hs
{-
 - Try to parse
     1: ([37.788353, -122.387695], [37.829853, -122.294312])
     2: ([37.429615, -122.087631], [37.487391, -122.018967])
     3: ([37.474858, -122.131577], [37.529332, -122.056046])
     4: ([37.532599,-122.218094], [37.615863,-122.097244])
     5: ([37.516262,-122.198181], [37.653383,-122.151489])
     6: ([37.504824,-122.181702], [37.633266,-122.121964])
 -}

import Text.ParserCombinators.Parsec

pLine = do
    idx <- pIndex
    char ':'
    spaces
    pts <- pPoints
    return (idx, pts)

pIndex = many digit
pPoints = do
    char '('
    pts <- sepBy pPoint (char ',')
    char ')'
    return pts

pPoint = do
    spaces
    char '['
    x <- many (noneOf ",")
    char ','
    y <- many (noneOf "]")
    char ']'
    spaces
    return (x,y)


readPoint :: (String, String) -> (Double, Double)
readPoint (x,y) = (read x, read y)

parseLine :: String -> (Int, [(Double,Double)])
parseLine input = case parse pLine "(error!)" input of
    Left _ -> error "Badly formatted example"
    Right (idx, pts) -> (read idx :: Int, map readPoint pts)

foo = "1: ([37.788353, -122.387695], [37.829853, -122.294312])"
