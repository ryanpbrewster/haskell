-- simple_calculator.hs
{-
 -  The goal of this challenge is to create a simple calculator.
 -
 -  The following operations should be supported with their order (operator
 -  precedence):
 -
 -  1   ()       Brackets
 -  2   -        Unary minus
 -  3   ^        Exponent
 -  4   *, /     Multiply, Divide (left-to-right precedence)
 -  5   +, -     Add, Subtract (left-to-right precedence)
 -
 -  Input sample:
 -
 -  Your program should accept as its first argument a path to a filename. The
 -  input file contains several lines. Each line is one test case. Each line
 -  contains mathematical expression. eg.
 -
 -  250*14.3
 -  3^6 / 117
 -  (2.16 - 48.34)^-1
 -  (59 - 15 + 3*6)/21
 -
 -  Output sample:
 -
 -  For each set of input produce a single line of output which is the result
 -  of calculation.
 -
 -  3575
 -  6.23077
 -  −0.02165
 -  2.95238
 -
 -  Note: Don't use any kind of eval function.
 -
 -  Constraints:
 -  Each number in input expression is greater than -20,000 and less than 20,000.
 -  Each output number is greater than -20,000 and less than 20,000.
 -  If output number is a float number it should be rounded to the 5th digit after the dot.
 -  E.g 14.132646 gets 14.13265, 14.132644 gets 14.13264, 14.132645 gets 14.13265.
 -
 -  If output number has less than 5 digits after the dot you don't need to add zeros.
 -  E.g. you need to print 16.34 (and not 16.34000) in case the answer is 16.34.
 -  And you need to print 16 (and not 16.00000) in case the answer is 16.
 -}

{-
 - I'm abusing Parsec here, since it's a convenient built-in parser.  This was
 - way more obnoxious than I expected it to be. Parsing arithmetic is
 - nontrivial.
 -}

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Monad
import Data.Maybe
import Data.Text (pack, unpack, strip)

import System.Environment (getArgs)
import Text.Printf

data Expr = Const (Either Integer Double)
          | Neg Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divide Expr Expr
          | Power Expr Expr

instance Show Expr where
    show (Const (Left x))  = show x
    show (Const (Right x)) = show x
    show (Neg x)      = "-" ++ show x
    show (Plus x y)   = "(" ++ show x ++ "+" ++ show y ++ ")"
    show (Minus x y)  = "(" ++ show x ++ "-" ++ show y ++ ")"
    show (Times x y)  = "(" ++ show x ++ "*" ++ show y ++ ")"
    show (Divide x y) = "(" ++ show x ++ "/" ++ show y ++ ")"
    show (Power x y)  = "(" ++ show x ++ "^" ++ show y ++ ")"

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
      , [Infix  (reservedOp "-" >> return Minus)  AssocLeft]
      , [Infix  (reservedOp "+" >> return Plus)   AssocLeft]
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

sanitize str = trim $ concat $ map (buffWithSpaces "+-*/^") str

trim = unpack . strip . pack

buffWithSpaces :: String -> Char -> String
buffWithSpaces set ch | ch `elem` set = ' ':ch:" "
                      | otherwise     = [ch]

parseAndEvaluate str =
    let syn_tree = case parse expr "" str of
                       Left err -> error $ show err
                       Right tr -> tr
    in evaluate syn_tree


-- This part is the obnoxious formatting that was requested
isInteger x = let r = fromIntegral $ round x
              in abs (x-r) < 1e-6

prepareOutput ans = roundToMinDecimalPlaces ans
roundToMinDecimalPlaces x =
    let rounds = tail $ take 6 $ iterate (10*) x
        r = 1e-5 * fromIntegral (round (1e5 * x))
    in fromJust $ lookup True [(isInteger x, show $ round x)
                              ,(any isInteger rounds, show r)
                              ,(True, printf "%.5f" x)
                              ]

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = map sanitize $ lines txt
                       anss = map parseAndEvaluate inputs
                       outputs = map prepareOutput anss
                   in unlines outputs
