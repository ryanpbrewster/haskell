-- Virus.hs
{-
 - Implementation of the virus game
 -}

import Graphics.Vty.Widgets.All
import Graphics.Vty
import qualified Data.Text as T
import Data.Array
import Data.List (intercalate)
import System.Exit (exitSuccess)
import System.Random

multilineString attr str = foldl1 (<->) $ map (string attr) (lines str)


{------------------------------------------------------------}
{- Underlying data structures: the board is a Grid of Cells -}
{------------------------------------------------------------}
data Cell = Cell Char deriving (Show, Eq, Ord)
cellChar (Cell ch) = ch

virusCellType = ' '
virus = Cell virusCellType
isVirus cell_type = cell_type == virus

data Grid = Grid (Array (Int,Int) Cell)
instance Show Grid where
    show (Grid arr) =
        let ((1,1),(rr,cc)) = bounds arr
            lns = [ map cellChar [arr ! (i,j)|j <- [1..cc]] | i <- [1..rr] ]
        in intercalate "\n" lns

inBounds (i,j) (rr,cc) = (1 <= i) && (i <= rr) && (1 <= j) && (j <= cc)
neighbors (i,j) (rr,cc) = filter (`inBounds` (rr,cc)) [ (i,j+1), (i-1,j), (i,j-1), (i+1,j) ]

infect :: Grid -> Cell -> Grid
(Grid arr) `infect` cell_type =
    let ((1,1), (rr,cc)) = bounds arr
        vulnerable = [ (i,j) | i <- [1..rr]
                             , j <- [1..cc]
                             , arr ! (i,j) == cell_type
                             , any (\idx -> arr ! idx == virus) (neighbors (i,j) (rr,cc)) ]
    in Grid $ arr // [ (idx, virus) | idx <- vulnerable ]

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
infectGridWidget wRef cell_type = updateWidgetState wRef (`infect` cell_type)


main = do
    let (rr,cc) = (15,45)
    rng <- getStdGen
    let rnds = map Cell $ randomRs ('a', 'c') rng
    let init_grid = Grid $ listArray ((1,1), (rr,cc)) $ virus : take (rr*cc-1) rnds

    runGame init_grid

runGame init_grid = do
    gw <- newGridWidget init_grid

    bgw <- bordered gw

    fg <- newFocusGroup
    fg `addToFocusGroup` bgw

    fg `onKeyPressed` \_ key _ ->
        case key of
            KEsc -> exitSuccess
            KASCII ch -> do gw `infectGridWidget` (Cell ch)
                            return True
            _ -> return False

    c <- newCollection
    showfg <- addToCollection c bgw fg

    runUi c defaultContext
