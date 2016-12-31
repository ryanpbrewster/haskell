-- parsec_example_2.hs
{-
 - General arithmetic parser
 -}

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Monad

data Expr = Const Integer | Plus Expr Expr | Minus Expr Expr deriving (Show)


lexer = Token.makeTokenParser emptyDef
reservedOp = Token.reservedOp lexer

ops = [ [Infix  (reservedOp "+" >> return Plus)  AssocLeft]
      , [Infix  (reservedOp "-" >> return Minus) AssocLeft]
      ]

integer = Token.integer lexer
term = liftM Const integer

expr = buildExpressionParser ops term

main = forever $ do
    ln <- getLine
    case parse expr "" ln of
        Left _ -> print "bad expr"
        Right tr -> print tr
