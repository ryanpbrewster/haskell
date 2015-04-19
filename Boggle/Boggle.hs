import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding (getChar)

{- Board class -}
type Location = (Int, Int)
data Cell = Cell { getChar :: Char
                 , getLocation :: (Int, Int)
                 } deriving (Ord, Eq)
instance Show Cell where
    show (Cell ch loc) = show ch ++ " @ " ++ show loc

class Board b where
    getCell   :: b -> Location -> Cell
    allCells  :: b -> [Cell]
    neighbors :: b -> Cell -> [Cell]

{- A HexBoard is a specific type of board -}
data HexBoard = HexBoard (A.Array Location Cell)
positionsInLayer l = if l == 0 then 1 else 6*l

newHexBoard :: [[Char]] -> HexBoard
newHexBoard layers =
    let max_l = length layers - 1
        max_p = (positionsInLayer max_l) - 1
        bounds = ((0,0), (max_l, max_p))
        cells = [ ((l,p), Cell ch (l,p)) | (l, layer) <- zip [0..] layers
                                       , (p, ch) <- zip [0..] layer ]
    in HexBoard $ A.array bounds cells

numLayers (HexBoard b) =
    let ((0,0), (max_l, max_p)) = A.bounds b
    in max_l + 1

instance Board HexBoard where
    getCell (HexBoard b) (l,p) =
        b A.! (l, p `mod` positionsInLayer l)
    allCells board@(HexBoard b) =
        [ getCell board (l,p) | l <- [0..numLayers board - 1]
                              , p <- [0..positionsInLayer l - 1] ]
    neighbors board@(HexBoard b) (Cell _ (l,p)) =
        let ns = hexNeighbors (l,p)
            legal = [ (l',p') | (l',p') <- ns, l' < numLayers board ]
        in map (getCell board) legal

onCorner (l,p) = p `mod` l == 0

hexNeighbors (0,0) = [(1,p) | p <- [0..5]]
hexNeighbors (l,p)
    | onCorner (l,p) =
        let r_out = (l+1) * p `div` l -- the location radially outwards
            r_in = (l-1) * p `div` l -- location radially inwards
            exterior = [ (l+1, p') | p' <- [r_out-1, r_out, r_out+1] ]
            in_layer = [ (l,   p') | p' <- [p-1, p+1] ]
            interior = [ (l-1, p') | p' <- [r_in] ]
        in exterior ++ in_layer ++ interior
    | otherwise      =
        let r_out = (l+1) * p `div` l -- the location radially outwards
            r_in = (l-1) * p `div` l -- location radially inwards
            exterior = [ (l+1, p') | p' <- [r_out, r_out+1] ]
            in_layer = [ (l,   p') | p' <- [p-1, p+1] ]
            interior = [ (l-1, p') | p' <- [r_in, r_in+1] ]
        in exterior ++ in_layer ++ interior


testboard = [ "A"
            , "BCDEFG"
            , "UANTCASTYSWQ"
            , "EORNOTOBEKANGARTOB"
            , "LUYAGIMMXVRHPJITSOOTHEPZ"
            ]

{- Trie -}
data TrieNode = TrieNode { getChildren :: M.Map Char TrieNode
                         , isValid :: Bool
                         } deriving (Show)

empty = TrieNode M.empty False
safeLookup = M.findWithDefault empty

-- If you insert an empty word then the current TrieNode must be at the end
-- of a valid suffix, so mark it as true
insert (TrieNode nm valid) [] = TrieNode nm True
-- To insert the rest of a suffix, recursively add it to the appropriate child
insert (TrieNode nm valid) (x:xs) =
    let child' = insert (safeLookup x nm) xs
    in TrieNode (M.insert x child' nm) valid

newTrie :: [String] -> TrieNode
newTrie = foldl insert empty

allWords :: TrieNode -> [String]
allWords (TrieNode nm valid) =
    let suffs = [ x : suff | (x, child) <- M.toList nm, suff <- allWords child ]
        empty_string = if valid then [""] else []
    in empty_string ++ suffs


{- Boggle Solver -}
boggleSolver :: Board b => b -> TrieNode -> S.Set String
boggleSolver board dict =
    let cells = allCells board
    in S.unions [ boggleSolverHelper c dict S.empty | c <- cells ]
  where boggleSolverHelper c@(Cell ch loc) (TrieNode nm valid) used
            | not (ch `M.member` nm) = S.empty
            | otherwise =
                let tr' = nm M.! ch
                    -- ^-- First, descend down the Trie
                    ns  = [ n | n <- neighbors board c, not (n `S.member` used) ]
                    -- ^-- Find all the unused neighbors
                    cur = if isValid tr' then S.singleton [ch] else S.empty
                    -- ^-- If the Trie indicates that we're at a valid word upon entering
                    used' = S.insert c used
                    others = [ S.map (ch:) $ boggleSolverHelper n tr' used' | n <- ns ]
                in S.unions (cur : others)

main = do
    [board_filename, dict_filename] <- getArgs
    board <- fmap (newHexBoard . lines) (readFile board_filename)
    dict  <- fmap (newTrie     . lines) (readFile dict_filename)
    mapM_ putStrLn $ S.toList $ boggleSolver board dict
