-- stack_implementation.hs
{-
 - Write a program implementing a stack inteface for integers.The interface
 - should have 'push' and 'pop' functions. You will be asked to 'push'
 - a series of integers and then 'pop' and print out every alternate integer.
 - Input sample:
 -
 - The first argument will be a text file containing a series of space
 - delimited integers, one per line. e.g.
 -
 - 1 2 3 4
 - 10 -2 3 4
 -
 - Output sample:
 -
 - Print to stdout, every alternate integer(space delimited), one per line.
 - e.g.
 -
 - 4 2
 - 4 -2
 -}


import System.Environment (getArgs)
import Data.Maybe

data Stack a = Stack a (Stack a) | Nil deriving (Show)

nil :: Stack a -> Bool
nil Nil = True
nil _   = False

pop :: Stack a -> (a, Stack a)
pop (Stack x st') = (x, st')

push :: a -> Stack a -> Stack a
push x st = Stack x st

fromList :: [a] -> Stack a
fromList xs = fromList' xs Nil
    where fromList' []     st = st
          fromList' (x:xs) st = fromList' xs (push x st)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = [ map read $ words ln | ln <- lines txt ]
                       ans = map popAlternates inps
                       outputs = map (unwords . map show) ans
                   in unlines outputs

popAlternates :: [Int] -> [Int]
popAlternates xs = let st = fromList xs
                   in popAlternates' st
    where popAlternates' Nil = []
          popAlternates' stk = let (x, stk') = pop stk
                                   stk'' = if nil stk' then Nil else snd $ pop stk'
                               in x : (popAlternates' stk'')
