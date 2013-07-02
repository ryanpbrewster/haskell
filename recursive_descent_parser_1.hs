-- recursive_descent_parser_1.hs
{-
 - A simple recursive descent parser for the language
 -     S -> VS | ""
 -     V -> (S) | [S] | {S}
 -
 -  That is, the language of all properly formed parenthesis-like strings
 -}

import Data.Maybe

data Expr = S | V deriving (Show)
data S = SC V S | Nil
data V = VC Char S Char

instance Show V where
    show v = showV v
instance Show S where
    show s = showS s

showV (VC l s r) = [l] ++ show s ++ [r]
showS Nil = ""
showS (SC v s) = show v ++ show s

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
