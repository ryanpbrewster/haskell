-- parsec_example_3.hs
{-
 - General arithmetic parser with parens
 -}

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Monad

data Expr = Const Integer
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divide Expr Expr
          | Power Expr Expr
          deriving (Show)


lexer = Token.makeTokenParser emptyDef
reservedOp = Token.reservedOp lexer

ops = [ [Infix  (reservedOp "^" >> return Power)  AssocLeft]
      , [Infix  (reservedOp "*" >> return Times)  AssocLeft]
      , [Infix  (reservedOp "/" >> return Divide) AssocLeft]
      , [Infix  (reservedOp "+" >> return Plus)   AssocLeft]
      , [Infix  (reservedOp "-" >> return Minus)  AssocLeft]
      ]

integer = Token.integer lexer
parens = Token.parens lexer

expr = buildExpressionParser ops term
term = parens expr <|> liftM Const integer


main = forever $ do
    ln <- getLine
    case parse expr "" ln of
        Left _ -> print "bad expr"
        Right tr -> print tr
