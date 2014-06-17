-- GRPH.hs
{-
 - Problem
 -
 - A graph whose nodes have all been labeled can be represented by an adjacency list, in which each row of the list contains the two node labels corresponding to a unique edge.
 -
 - A directed graph (or digraph) is a graph containing directed edges, each of which has an orientation. That is, a directed edge is represented by an arrow instead of a line segment; the starting and ending nodes of an edge form its tail and head, respectively. The directed edge with tail v and head w is represented by (v,w) (but not by (w,v)). A directed loop is a directed edge of the form (v,v).
 -
 - For a collection of strings and a positive integer k, the overlap graph for the strings is a directed graph Ok in which each string is represented by a node, and string s is connected to string t with a directed edge when there is a length k suffix of s that matches a length k prefix of t, as long as s≠t; we demand s≠t to prevent directed loops in the overlap graph (although directed cycles may be present).
 -
 - Given: A collection of DNA strings in FASTA format having total length at most 10 kbp.
 -
 - Return: The adjacency list corresponding to O3. You may return edges in any order.
 - Sample Dataset
 -
 - >Rosalind_0498
 - AAATAAA
 - >Rosalind_2391
 - AAATTTT
 - >Rosalind_2323
 - TTTTCCC
 - >Rosalind_0442
 - AAATCCC
 - >Rosalind_5013
 - GGGTGGG
 -
 - Sample Output
 -
 - Rosalind_0498 Rosalind_2391
 - Rosalind_0498 Rosalind_0442
 - Rosalind_2391 Rosalind_2323
 -}


import System.Environment (getArgs)
import qualified Data.Array as A
import qualified Data.Map as M
import Data.List (intercalate, maximumBy, transpose)
import Data.Ord (comparing)

import Rosalind.Structures
import Rosalind.Parsing (parseFASTAs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let fastas = parseFASTAs txt
                       adj_list = makeAdjacencyList fastas
                       outs = [ showID (getID a) ++ " " ++ showID (getID b) | (a,b) <- adj_list ]
                   in unlines outs

makeAdjacencyList xs = filter (overlap 3) [ (a,b) | a <- xs, b <- xs, a /= b ]

overlap k (a,b) = let suff = takeR k $ getNCLs $ getDNA a
                      pref = takeL k $ getNCLs $ getDNA b
                  in suff == pref

takeL n xs = take n xs
takeR n xs = drop (length xs - n) xs
