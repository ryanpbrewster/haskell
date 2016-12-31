import Control.Monad.State
import qualified Data.Map as M

data Node a = Leaf a
            | Node (Node a) (Node a) deriving (Show)

foo = Node (Leaf 'f') (Node (Leaf 'o') (Leaf 'o'))

-- The brute-force functional way, involving explicit passing of state
bfLabel :: Node a -> Node Int
bfLabel = snd . bfLabel' 1
    where bfLabel' idx (Leaf _) = (idx+1, Leaf idx)
          bfLabel' idx (Node l r) =
              let (idx',  l') = bfLabel' idx  l
                  (idx'', r') = bfLabel' idx' r
              in (idx'', Node l' r')



-- Using the State monad
-- Interact with the current state using {get, put, ...}

labelNode n = evalState (return n >>= label) 1

label (Leaf _) = do
    idx <- get
    put (idx+1)
    return $ Leaf idx
label (Node l r) = do
    l' <- label l
    r' <- label r
    return $ Node l' r'



{-
 - Now let's do the same exploration with a Trie
 -}

data TrieNode = TrieNode (M.Map Char TrieNode) deriving (Show)
data TrieLabelNode = TrieLabelNode Int (M.Map Char TrieLabelNode) deriving (Show)

bar = newTrie ["abc", "abd", "qrs"]

empty = TrieNode M.empty

newTrie :: [String] -> TrieNode
newTrie = foldl (flip insert) empty

insert :: String -> TrieNode -> TrieNode
insert [] n = n
insert (x:xs) (TrieNode nm) = let n' = M.findWithDefault empty x nm
                              in TrieNode $ M.insert x (insert xs n') nm

labelTrie (TrieNode nm) = do
    idx <- get
    put (idx+1)
    tns' <- mapM labelTrie $ M.elems nm
    let nm' = M.fromList $ zip (M.keys nm) tns'
    return $ TrieLabelNode idx nm'
