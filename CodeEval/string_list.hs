-- string_list.hs
{-
 - You are given a number N and a string S. Print all of the possible ways to
 - write a string of length N from the characters in string S, comma delimited
 - in alphabetical order.
 - Input sample:
 -
 - The first argument will be the path to the input filename containing the
 - test data. Each line in this file is a separate test case. Each line is in
 - the format: N,S i.e. a positive integer, followed by a string (comma
 - separated) eg.
 -
 - 1,aa
 - 2,ab
 - 3,pop
 -
 - Output sample:
 -
 - Print all of the possible ways to write a string of length N from the
 - characters in string S comma delimited in alphabetical order, with no
 - duplicates. eg.
 -
 - a
 - aa,ab,ba,bb
 - ooo,oop,opo,opp,poo,pop,ppo,ppp
 -}


import System.Environment (getArgs)
import Data.List (nub, sort, intercalate)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

parseLine s = let (l,r) = break (==',') s
              in (read l, sort $ nub $ tail r)

tuples 0 _ = [[]]
tuples k xs = [ x:t | x <- xs, t <- tuples (k-1) xs ]

solveProblem txt = let inputs = [ parseLine ln | ln <- lines txt ]
                       anss = [ tuples k set | (k,set) <- inputs ]
                       outputs = [ intercalate "," ans | ans <- anss ]
                   in unlines outputs
