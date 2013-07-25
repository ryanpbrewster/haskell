-- parsec_example_1.hs
{-
 - Do (+), then (*)
 -}

import Text.ParserCombinators.Parsec

line = adds

adds = do first <- add
          next <- moreAdds
          return (first:next)

moreAdds = (char '+' >> adds)
       <|> (return [])

add = prods

prods = do first <- prod
           next <- moreProds
           return (first:next)

prod :: GenParser Char st Int
prod = do spaces
          ans <- many digit
          spaces
          return (read ans)

moreProds = (char '*' >> prods)
        <|> (return [])

parseLine input = parse line "(unknown)" input


evaluate tree = sum $ map product tree
