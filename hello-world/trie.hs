-- trie.hs

import qualified Data.Map as M
import System.Environment (getArgs)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)

data Node = Node { getMap :: M.Map Char Node
                 } deriving (Show)
showNode (Node nm) =
    "[" ++
    intercalate "," [ show x ++ " -> " ++ showNode n' | (x,n') <- M.toList nm ] ++
    "]"

empty = Node M.empty

safeLookup = M.findWithDefault empty
singleton xx = insert xx empty

newTrie :: [String] -> Node
newTrie = foldl (flip insert) empty

-- indicate that a word is complete with a '$'
insert []     (Node nm) = Node $ M.insert '$' empty nm
insert (x:xs) (Node nm) = let n' = safeLookup x nm
                          in Node $ M.insert x (insert xs n') nm

descend :: Node -> String -> Node
descend n "" = n
descend (Node nm) (c:cs) = descend (safeLookup c nm) cs

descendAll :: Node -> Int -> M.Map String Node
descendAll n 0 = M.singleton "" n
descendAll (Node nm) k =
    M.unions [ M.mapKeys (c:) (descendAll n' (k-1)) | (c,n') <- M.toList nm ]

size (Node nm) = 1 + sum [ size n' | n' <- M.elems nm ]

wordCount :: Node -> Int
wordCount (Node nm) =
    let is_complete = '$' `M.member` nm
    in (if is_complete then 1 else 0) + (sum $ map wordCount $ M.elems nm)


main = do
    txt <- readFile "/home/rbrewster/misc/long/words.txt"
    let wrds = lines txt
    let tr = newTrie wrds
    let trs = descendAll tr 3
    print $ sortBy (comparing snd) $ M.toList $ M.map wordCount trs
