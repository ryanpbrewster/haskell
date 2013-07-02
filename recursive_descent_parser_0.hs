-- recursive_descent_parser_0.hs
{-
 - A simple recursive descent parser for the almost-trivial language
 -     S -> VS | ""
 -     V -> (S)
 -
 -  That is, the language of all properly formed parenthesis strings
 -}

import Data.Maybe

data Expr = S | V deriving (Show)
data S = SC V S | Nil
data V = VC S

instance Show V where
    show v = showV v
instance Show S where
    show s = showS s

showV (VC s) = "(" ++ show s ++ ")"
showS Nil = ""
showS (SC v s) = show v ++ show s

parseS str = let vp = parseV str
             in case vp of
                Just (v,rest) -> let (s,fin) = parseS rest in (SC v s, fin)
                Nothing -> (Nil, str)


-- parseV returns <Just (v, rest)> if it can successfully pull a V from str
-- Otherwise, it returns Nothing
parseV ('(':str) = let (s,rest) = parseS str
                   in case rest of
                       (')':fin) -> Just (VC s, fin)
                       _ -> Nothing
parseV _ = Nothing

valid str = let (s,rest) = parseS str in rest == ""
