-- parsec_example_4.hs
{-
 - Simple calculator
 -}

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Monad

data Expr = Const (Either Integer Double)
          | Neg Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divide Expr Expr
          | Power Expr Expr
          deriving (Show)


lexer = Token.makeTokenParser emptyDef
reservedOp = Token.reservedOp lexer
num = Token.naturalOrFloat lexer
parens = Token.parens lexer
expr = buildExpressionParser ops term
term = parens expr <|> liftM Const num

ops = [ [Prefix (reservedOp "-" >> return Neg)             ]
      , [Infix  (reservedOp "^" >> return Power)  AssocLeft]
      , [Infix  (reservedOp "*" >> return Times)  AssocLeft]
      , [Infix  (reservedOp "/" >> return Divide) AssocLeft]
      , [Infix  (reservedOp "+" >> return Plus)   AssocLeft]
      , [Infix  (reservedOp "-" >> return Minus)  AssocLeft]
      ]


evaluate :: Expr -> Double
evaluate (Const (Left x)) = fromIntegral x
evaluate (Const (Right x)) = x
evaluate (Neg x)      = -(evaluate x)
evaluate (Plus x y)   = (evaluate x) + (evaluate y)
evaluate (Minus x y)  = (evaluate x) - (evaluate y)
evaluate (Times x y)  = (evaluate x) * (evaluate y)
evaluate (Divide x y) = (evaluate x) / (evaluate y)
evaluate (Power x y)  = (evaluate x) ** (evaluate y)

main = forever $ do
    ln <- getLine
    case parse expr "" ln of
        Left e -> print e
        Right tr -> print $ evaluate tr
