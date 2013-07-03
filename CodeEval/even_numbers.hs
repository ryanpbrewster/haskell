-- even_numbers.hs
{-
 - Write a program which checks input numbers and determines whether a number is even or not.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Input example is the following
 -
 - 701
 - 4123
 - 2936
 - Output sample:
 -
 - Print 1 if the number is even or 0 if the number is odd.
 -
 - 0
 - 0
 - 1
 -}

{-
 - <even> is a builtin predicate in Haskell
 - it could easily be implemented as
 -     even x = (x `mod` 2) == 0
 -}
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = map read $ lines txt
                       anss = map even inputs
                       outputs = [ if a then "1" else "0" | a <- anss ]
                   in unlines outputs
