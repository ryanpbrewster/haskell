-- Virus.hs
{-
 - Implementation of the virus game
 -}

import Graphics.Vty.Widgets.All
import Graphics.Vty
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Array
import Data.List (intercalate)
import System.Exit (exitSuccess)
import System.Random

multilineString attr str = foldl1 (<->) $ map (string attr) (lines str)


{------------------------------------------------------------}
{- Underlying data structures: the board is a Grid of Cells -}
{------------------------------------------------------------}
data Cell = Alive | Dead deriving (Show, Eq, Ord)
cellChar Alive = 'O'
cellChar Dead  = '.'

cellValue Alive = 1
cellValue Dead  = 0

data Grid = Grid (Array (Int,Int) Cell)
instance Show Grid where
    show (Grid arr) =
        let ((1,1),(rr,cc)) = bounds arr
            lns = [ map cellChar [arr ! (i,j)|j <- [1..cc]] | i <- [1..rr] ]
        in intercalate "\n" lns

inBounds (i,j) ((rlo,clo),(rhi,chi)) =
    (rlo <= i) && (i <= rhi) && (clo <= j) && (j <= chi)

neighbors (i,j) bounds =
    filter (`inBounds` bounds) [(i-1,j-1),(i-1,j),(i-1,j+1)
                               ,(i  ,j-1)        ,(i  ,j+1)
                               ,(i+1,j-1),(i+1,j),(i+1,j+1)]

liveNeighbors (i,j) (Grid arr) =
    sum $ map (cellValue . (arr!)) $ neighbors (i,j) (bounds arr)

updateCell idx g@(Grid arr) =
    let status = arr ! idx
        count = liveNeighbors idx g
    in case status of
        Alive -> if (count < 2 || count > 3) then Dead else Alive
        Dead  -> if count == 3 then Alive else Dead

updateGrid g@(Grid arr) =
    Grid $ arr // [ (idx, updateCell idx g) | idx <- indices arr ]

rpentominoGrid bds@((1,1),(rr,cc)) =
    let (i, j) = (rr `quot` 2, cc `quot` 2)
        rpentomino_locs = [          (i-1,j), (i-1,j+1)
                          , (i,j-1), (i,  j)
                          ,          (i+1,j)
                          ]
        dead_grid = accumArray const Dead bds []
    in Grid $ dead_grid // zip rpentomino_locs (repeat Alive)


{--------------------------------------------------------------}
{- New Widget definition for displaying and updating the grid -}
{--------------------------------------------------------------}
-- all the grid widget needs to know how to do is display a grid
newGridWidget init_grid = do
    newWidget init_grid $ \w ->
        w { render_ = \this size ctx -> do
                g <- getState this
                return $ multilineString (getNormalAttr ctx) (show g)
          }

-- define how to update the grid
updateGridWidget wRef = updateWidgetState wRef updateGrid


{-------------------------------------------------------------}
{- Main driver. Initialize a random grid, then run the game. -}
{-------------------------------------------------------------}
main = do
    let (rr,cc) = (15,45)
    let init_grid = rpentominoGrid ((1,1), (rr,cc))
    runGame init_grid

runGame init_grid = do
    gw <- newGridWidget init_grid
    bgw <- bordered gw

    fg <- newFocusGroup
    fg `addToFocusGroup` bgw

    fg `onKeyPressed` \_ key _ ->
        case key of
            KEsc -> exitSuccess
            _    -> updateGridWidget gw >> return True

    c <- newCollection
    _ <- addToCollection c bgw fg

    runUi c defaultContext
