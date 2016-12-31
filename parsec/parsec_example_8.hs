-- parsec_example_8.hs
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
    idx <- many digit
    char ':'
    spaces
    string "(["
    x1 <- many (noneOf ",")
    char ','
    y1 <- many (noneOf "]")
    string "],"
    spaces
    char '['
    x2 <- many (noneOf ",")
    char ','
    y2 <- many (noneOf "]")
    string "])"
    return (idx, (x1,y1), (x2,y2))

parseLine :: String -> (Int, (Double,Double), (Double,Double))
parseLine input = case parse pLine "(error!)" input of
    Left _ -> error "Badly formatted example"
    Right v -> readInput v

readInput (idx, (x1,y1), (x2,y2)) =
    (read idx, (read x1, read y1), (read x2, read y2))

foo = "1: ([37.788353, -122.387695], [37.829853, -122.294312])"
