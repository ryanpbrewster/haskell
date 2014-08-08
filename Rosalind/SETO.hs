-- SETO.hs
{-
 - Problem
 -
 - If A and B are sets, then their union A∪B is the set comprising any elements in either A or B; their intersection A∩B is the set of elements in both A and B; and their set difference A−B is the set of elements in A but not in B.
 -
 - Furthermore, if A is a subset of another set U, then the set complement of A with respect to U is defined as the set Ac=U−A. See the Sample sections below for examples.
 -
 - Given: A positive integer n (n≤20,000) and two subsets A and B of {1,2,…,n}.
 -
 - Return: Six sets: A∪B, A∩B, A−B, B−A, Ac, and Bc (where set complements are taken with respect to {1,2,…,n}).
 - Sample Dataset
 -
 - 10
 - {1, 2, 3, 4, 5}
 - {2, 8, 5, 10}
 -
 - Sample Output
 -
 - {1, 2, 3, 4, 5, 8, 10}
 - {2, 5}
 - {1, 3, 4}
 - {8, 10}
 - {8, 9, 10, 6, 7}
 - {1, 3, 4, 6, 7, 9}
 -}

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import qualified Data.Set as S
import Control.Applicative ((*>))

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let (n, a, b) = parseInput txt
                       omega = S.fromList [1..n]
                       anss = [ a `S.union` b
                              , a `S.intersection` b
                              , a `S.difference` b
                              , b `S.difference` a
                              , omega `S.difference` a
                              , omega `S.difference` b
                              ]
                       outs = map formatOutput anss
                   in unlines outs

parseInput :: String -> (Int, S.Set Int, S.Set Int)
parseInput str = case parse pInput "" str of
    Right v -> v
    Left _  -> error $ "Improperly formatted input: " ++ show str

pInput = do
    n <- pInt
    a <- pSet
    b <- pSet
    return (n, a, b)

pInt = do
    spaces
    str <- many digit
    return $ read str

pSet = do
    spaces
    char '{'
    xs <- pInt `sepBy` (char ',')
    char '}'
    return $ S.fromList xs

formatOutput s = "{" ++ intercalate ", " (map show $ S.elems s) ++ "}"
