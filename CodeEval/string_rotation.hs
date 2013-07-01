-- string_rotation.hs
{-
 - You are given two strings. Determine if the second string is a rotation of
 - the first string.
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains two comma separated strings. e.g.
 -
 - Hello,lloHe
 - Basefont,tBasefon
 -
 - Output sample:
 -
 - Print out True/False if the second string is a rotation of the first. e.g.
 -
 - True
 - True
 -}


import System.Environment (getArgs)
import Data.List (tails, inits)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = [ wordsBy "," ln | ln <- lines txt ]
                       anss = [ b `isRotationOf` a | [a,b] <- inps ]
                   in unlines $ map show anss

b `isRotationOf` a = let rots = zipWith (++) (tails a) (inits a)
                     in b `elem` rots

wordsBy delims s = wordsBy' delims s
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)
