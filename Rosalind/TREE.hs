-- TREE.hs
{-
 - An undirected graph is connected if there is a path connecting any two
 - nodes. A tree is a connected (undirected) graph containing no cycles; this
 - definition forces the tree to have a branching structure organized around
 - a central core of nodes, just like its living counterpart. See Figure 2.
 -
 - We have already grown familiar with trees in “Mendel's First Law”, where we
 - introduced the probability tree diagram to visualize the outcomes of
 - a random variable.
 -
 - In the creation of a phylogeny, taxa are encoded by the tree's leaves, or
 - nodes having degree 1. A node of a tree having degree larger than 1 is
 - called an internal node.
 -
 - Given: A positive integer n (n≤1000) and an adjacency list corresponding to
 - a graph on n nodes that contains no cycles.
 -
 - Return: The minimum number of edges that can be added to the graph to
 - produce a tree.
 -
 - Sample Dataset
 -     10
 -     1 2
 -     2 8
 -     4 10
 -     5 9
 -     6 10
 -     7 9
 -
 - Sample Output
 -     3
 -}

{-
 - So I'm pretty sure that you just figure out how many connected components
 - there are (aka the number of trees in the forest we are presented). The
 - answer is just n-1, where n is the number of components.
 -
 - Data.Graph includes functions that do this for you.
 -}

{-
 - We're guaranteed that none of the components have cycles. That necessarily
 - means that the full tree is minimally connected, and thus must have
 -     |E| = |V|-1
 - Just figure out how many edges need to be added to bring it up to that
 - total.
 -     required_edges = |V| - 1 - |E_0|
 -}


import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let (v:es) = lines txt
                       ans = (read v) - 1 - length es
                   in show ans
