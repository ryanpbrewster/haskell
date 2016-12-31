-- tree_level_sum.hs
{-
Given a tree, find the sum of all the elements at height H
-}

data Tree a = Tree a [Tree a] deriving (Show)

getChildren (Tree _ children) = children
getValue (Tree v _) = v

type Height = Int

-- porky is so-named because Richard came up with this solution
porky :: Tree Int -> Height -> Int
porky (Tree v children) 0 = v
porky (Tree _ children) h = sum [ porky child (h-1) | child <- children ]

-- This is the "breadth-first" traversal of the tree
bfs :: Tree Int -> Height -> Int
bfs tree h = 
  let sumsByLevel = map (sum . map getValue) (levels tree)
  in sumsByLevel !! h

levels :: Tree a -> [[Tree a]]
levels tree = iterate (concatMap getChildren) [tree]
