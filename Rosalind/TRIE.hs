-- TRIE.hs
{-
 - Given a collection of strings, their trie (often pronounced "try" to avoid
 - ambiguity with the general term tree) is a rooted tree formed as follows.
 - For every unique first symbol in the strings, an edge is formed connecting
 - the root to a new vertex. This symbol is then used to label the edge.
 -
 - We may then iterate the process by moving down one level as follows. Say
 - that an edge connecting the root to a node v is labeled with 'A'; then we
 - delete the first symbol from every string in the collection beginning with
 - 'A' and then treat v as our root. We apply this process to all nodes that
 - are adjacent to the root, and then we move down another level and continue.
 - See Figure 1 for an example of a trie.
 -
 - As a result of this method of construction, the symbols along the edges of
 - any path in the trie from the root to a leaf will spell out a unique string
 - from the collection, as long as no string is a prefix of another in the
 - collection (this would cause the first string to be encoded as a path
 - terminating at an internal node).
 -
 - Given: A list of at most 100 DNA strings of length at most 100 bp, none of
 - which is a prefix of another.
 -
 - Return: The adjacency list corresponding to the trie T for these patterns,
 - in the following format. If T has n nodes, first label the root with 1 and
 - then label the remaining nodes with the integers 2 through n in any order
 - you like. Each edge of the adjacency list of T will be encoded by a triple
 - containing the integer representing the edge's parent node, followed by the
 - integer representing the edge's child node, and finally the symbol labeling
 - the edge.
 -
 - Sample Dataset
 -
 - ATAGA
 - ATC
 - GAT
 -
 - Sample Output
 -
 - 1 2 A
 - 2 3 T
 - 3 4 A
 - 4 5 G
 - 5 6 A
 - 3 7 C
 - 1 8 G
 - 8 9 A
 - 9 10 T
 -}

import qualified Data.Map as M
import System.Environment (getArgs)
import Data.List (intercalate)

data Node = Node (M.Map Char Node) deriving (Show)
showNode (Node nm) =
    "[" ++
    intercalate "," [ show x ++ " -> " ++ showNode n' | (x,n') <- M.toList nm ] ++
    "]"

empty = Node M.empty

singleton xx = insert xx empty

newTrie :: [String] -> Node
newTrie = foldl (flip insert) empty

insert [] n = n
insert (x:xs) (Node nm) = let n' = M.findWithDefault empty x nm
                          in Node $ M.insert x (insert xs n') nm

data LabelNode = LabelNode Int (M.Map Char LabelNode) deriving (Show)

label :: Node -> LabelNode
label n = snd $ label' 1 n
    where label' cur (Node nm) =
              let (cur', nm') = M.mapAccum label' (cur+1) nm
              in (cur', LabelNode cur nm')

labelNodeToDot :: LabelNode -> [String]
labelNodeToDot (LabelNode id nm) =
    let vertex = [ show id ++ ";" ]
        edges = [ show id ++ " -> " ++ show id' ++
                  "[label=" ++ (ch:"") ++ "];" | (ch, LabelNode id' _) <- M.toList nm ]
        rest = concat $ map labelNodeToDot $ M.elems nm
    in vertex ++ edges ++ rest

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let trie = newTrie (lines txt)
                   in unlines $ trieEdges $ label trie

trieEdges :: LabelNode -> [String]
trieEdges (LabelNode a nm) =
    let edges = [ show a ++ " " ++ show b ++ " " ++ [ch]
                    | (ch, LabelNode b _) <- M.toList nm ]
        rest = concat $ map trieEdges $ M.elems nm
    in edges ++ rest
