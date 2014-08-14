-- Virus.hs
{-
 - Implementation of the virus game
 -}

import Data.Array
import Data.List (intercalate)
import Prelude hiding (Left, Right)
import Graphics.Vty.Widgets.All
import Graphics.Vty
import qualified Data.Text as T
import Data.List (intercalate)
import System.Exit (exitSuccess)
import System.Random

dump _ x = x
{------------------------------------------------------------}
{- Underlying data structures: the board is a Grid of Tiles -}
{------------------------------------------------------------}
data Tile = Tile Integer deriving (Eq, Ord)
instance Show Tile where
    show (Tile v) = show v

data Direction = Up | Down | Left | Right deriving (Show)

data TilePicker = TilePicker Int deriving (Show)

data Grid = Grid (Array (Int,Int) Tile)
instance Show Grid where
    show (Grid arr) =
        let ((1,1),(rr,cc)) = bounds arr
            lns = [ [arr!(i,j)|j <- [1..cc]] | i <- [1..rr]]
        in intercalate "\n" $ map (show . map show) lns

emptyGrid (r,c) = Grid $ accumArray const (Tile 0) ((1,1),(r,c)) []
foo (r,c) = Grid $ accumArray dump (Tile 0) ((1,1),(r,c)) [((1,1), Tile 2), ((1,2), Tile 2)]

lineIndices :: (Int,Int) -> Direction -> [[(Int,Int)]]
lineIndices (r,c) Up    = [[(i,j)|i<-[1..r]]    |j<-[1..c]]
lineIndices (r,c) Down  = [[(i,j)|i<-[r,r-1..1]]|j<-[1..c]]
lineIndices (r,c) Left  = [[(i,j)|j<-[1..c]]    |i<-[1..r]]
lineIndices (r,c) Right = [[(i,j)|j<-[c,c-1..1]]|i<-[1..r]]

-- Breaks a grid into a list-of-lists along the given direction
breakGridIntoLines :: Grid -> Direction -> [[Tile]]
breakGridIntoLines (Grid arr) dir =
    let ((1,1),(r,c)) = bounds arr
    in [ map (arr !) idxs | idxs <- lineIndices (r,c) dir ]

-- Takes in some bounds and a list-of-lists of Tiles and reconstructs the Grid
buildGridFromLines :: (Int,Int) -> [[Tile]] -> Direction -> Grid
buildGridFromLines (r,c) lns dir =
    let ln_idxs = lineIndices (r,c) dir
        tiles = concat [ zip idxs ln | (idxs, ln) <- zip ln_idxs lns ]
    in Grid $ accumArray dump (Tile 0) ((1,1),(r,c)) tiles


combineTiles (Tile x) (Tile y) = Tile (x+y)

collapseLine [] = []
collapseLine [x] = [x]
collapseLine (x:y:xs)
    | x == y    = combineTiles x y : collapseLine xs
    | otherwise = x : collapseLine (y:xs)

moveTiles :: Grid -> Direction -> Grid
moveTiles g@(Grid arr) dir =
    let ((1,1),(r,c)) = bounds arr
        lns = breakGridIntoLines g dir
        lns' = map collapseLine lns
    in buildGridFromLines (r,c) lns' dir

updateGrid :: Grid -> Direction -> TilePicker -> Grid
updateGrid g dir tp = addTile (moveTiles g dir) tp

addTile :: Grid -> TilePicker -> Grid
addTile (Grid arr) tp =
    let empty_tiles = filter (\idx -> arr ! idx == Tile 0) (indices arr)
    in Grid $ arr // [(tp `pickOne` empty_tiles, Tile 2)]

pickOne :: TilePicker -> [a] -> a
pickOne (TilePicker i) xs = xs !! (i `mod` length xs)








{--------------------------------------------------------------}
{- New Widget definition for displaying and updating the grid -}
{--------------------------------------------------------------}

multilineString attr str = foldl1 (<->) $ map (string attr) (lines str)

-- all the grid widget needs to know how to do is display a grid
newGridWidget init_grid = do
    newWidget init_grid $ \w ->
        w { render_ = \this size ctx -> do
                g <- getState this
                return $ multilineString (getNormalAttr ctx) (show g)
          }

-- define how to update the grid
updateGridWidget wRef dir = do
    r <- randomIO :: IO Int
    updateWidgetState wRef $ \g -> updateGrid g dir (TilePicker r)


{-------------------------------------------------------------}
{- Main driver. Initialize a random grid, then run the game. -}
{-------------------------------------------------------------}
main = do
    let init_grid = foo (4,4)
    runGame init_grid

runGame init_grid = do
    gw <- newGridWidget init_grid
    bgw <- bordered gw

    fg <- newFocusGroup
    fg `addToFocusGroup` bgw

    fg `onKeyPressed` \_ key _ ->
        case key of
            KEsc      -> exitSuccess
            KUp    -> updateGridWidget gw Up    >> return True
            KDown  -> updateGridWidget gw Down  >> return True
            KLeft  -> updateGridWidget gw Left  >> return True
            KRight -> updateGridWidget gw Right >> return True
            _     -> return False

    c <- newCollection
    _ <- addToCollection c bgw fg

    runUi c defaultContext
