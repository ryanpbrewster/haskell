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

inBounds (i,j) ((rlo,clo),(rhi,chi)) = (rlo <= i) && (i <= rhi) && (clo <= j) && (j <= chi)
neighbors (i,j) bounds = filter (`inBounds` bounds) [ (i,j+1), (i-1,j), (i,j-1), (i+1,j) ]

infect :: Grid -> Cell -> Grid
g@(Grid arr) `infect` cell_type = Grid $ arr // [ (idx,virus) | idx <- bfsVulnerableCells g cell_type ]

-- bfs through the grid and find all of the vulnerable cells
-- Vulnerability is defined as those cells of type cell_type that are next to
-- a virus or next to a vulnerable cell
bfsVulnerableCells :: Grid -> Cell -> [(Int,Int)]
bfsVulnerableCells (Grid arr) cell_type = let infected = head $ filter (isVirus . (arr!)) (indices arr)
                                          in bfs [infected] S.empty []
    where isVuln ct = ct == virus || ct == cell_type
          bfs []     vis vuln = vuln
          bfs (x:xs) vis vuln
              | x `S.member` vis = bfs xs vis vuln
              | otherwise        =
                  let ns = neighbors x (bounds arr)
                      vuln_ns = [ n | n <- ns, isVuln (arr ! n) ]
                      vuln' = if (arr!x) == cell_type then (x:vuln) else vuln
                  in bfs (vuln_ns ++ xs) (S.insert x vis) vuln'

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


{-------------------------------------------------------------}
{- Main driver. Initialize a random grid, then run the game. -}
{-------------------------------------------------------------}
main = do
    let (rr,cc) = (15,45)
    let cell_range = ('a', 'd')
    rng <- getStdGen
    let rnds = map Cell $ randomRs cell_range rng
    let init_grid = Grid $ listArray ((1,1), (rr,cc)) $ virus : take (rr*cc-1) rnds
    runGame init_grid

runGame init_grid = do
    gw <- newGridWidget init_grid
    bgw <- bordered gw

    fg <- newFocusGroup
    fg `addToFocusGroup` bgw

    fg `onKeyPressed` \_ key _ ->
        case key of
            KEsc      -> exitSuccess
            KASCII ch -> gw `infectGridWidget` (Cell ch) >> return True
            _         -> return False

    c <- newCollection
    _ <- addToCollection c bgw fg

    runUi c defaultContext
