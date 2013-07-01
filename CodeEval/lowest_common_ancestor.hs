-- lowest_common_ancestor.hs
{-
 - Write a program to determine the lowest common ancestor of two nodes in
 - a binary search tree. You may hardcode the following binary search tree in
 - your program:
 -
 -     30
 -     |
 -   ____
 -   |   |
 -   8   52
 -   |
 - ____
 - |   |
 - 3  20
 -     |
 -    ____
 -   |   |
 -   10 29
 -
 - Input sample:
 -
 - The first argument will be a text file containing 2 values that represent
 - two nodes within the tree, one per line. e.g.
 -
 - 8 52
 - 3 29
 -
 - Output sample:
 -
 - Print to stdout, the least common ancestor, one per line, e.g.
 -
 - 30
 - 8
 -}

import System.Environment (getArgs)


data Tree = Tree Int Tree Tree | Nil deriving (Show)

tree = Tree 30
            (Tree 8
                (Tree 3 Nil Nil)
                (Tree 20
                    (Tree 10 Nil Nil)
                    (Tree 29 Nil Nil)))
            (Tree 52 Nil Nil)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = [ map read $ words ln | ln <- lines txt ]
                       outputs = [ lca a b tree | [a,b] <- inps ]
                   in unlines $ map show outputs

lca a b (Tree t l r) | a <= t && b >= t = t
                     | b < t = lca a b l
                     | a > t = lca a b r
