-- suffix_tree.hs
-- An attempt to implement Suffix Trees in Haskell

import Data.List (tails, foldr1)
import qualified Data.Map as M
import Data.Maybe (maybe)

data SuffixTree = Node Bool (M.Map Char SuffixTree) deriving (Show)

-- this is crappy brute-force construction, I think O(N^2)
-- I know that there is an O(N) construction method, but I'm lazy
construct :: String -> SuffixTree
construct str = foldr1 union (map construct' $ tails str)
  where
  construct' [] = Node True M.empty
  construct' (x:xs) = Node False (M.singleton x (construct' xs))

union :: SuffixTree -> SuffixTree -> SuffixTree
(Node v1 es1) `union` (Node v2 es2) =
  Node (v1 || v2) (M.unionWith union es1 es2)

intersection :: SuffixTree -> SuffixTree -> SuffixTree
(Node v1 es1) `intersection` (Node v2 es2) =
  Node (v1 && v2) (M.intersectionWith intersection es1 es2)

size :: SuffixTree -> Int
size (Node v es) = sum (map size $ M.elems es) + if v then 1 else 0

count :: SuffixTree -> String -> Int
count t "" = size t
count (Node v es) (x:xs) = maybe 0 (\t -> count t xs) (M.lookup x es)

depth :: SuffixTree -> Int
depth (Node _ es) = if M.null es then 0 else 1 + maximum (map depth $ M.elems es)
