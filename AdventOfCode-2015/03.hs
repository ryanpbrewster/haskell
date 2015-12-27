-- 03.hs
import qualified Data.Set as S
import Data.List (transpose)
import Data.Maybe (mapMaybe)
main = do
  directions <- fmap (mapMaybe charToDirection) (readFile "03.input")
  print $ S.size $ distinctPositions directions
  print $ S.size $ distinctPositionsForKActors 2 directions

-- uncollate 3 [1,2,3,1,2,3,1,2,3] == [ [1,1,1], [2,2,2], [3,3,3] ]
uncollate k xs = transpose $ chunk k xs
chunk k [] = []
chunk k xs =
  let (f,r) = splitAt k xs
  in f : chunk k r

distinctPositions :: [Direction] -> S.Set Position
distinctPositions directions =
  S.fromList $ scanl move (Position (0,0)) directions

distinctPositionsForKActors :: Int -> [Direction] -> S.Set Position
distinctPositionsForKActors k directions =
  S.unions $ map distinctPositions $ uncollate k directions

data Direction = R | U | L | D
charToDirection '>' = Just R
charToDirection '^' = Just U
charToDirection '<' = Just L
charToDirection 'v' = Just D
charToDirection _ = Nothing

newtype Position = Position (Int, Int) deriving (Eq, Ord)
move :: Position -> Direction -> Position
move (Position (x, y)) dir =
  let (x', y') = case dir of
                   R -> (x+1, y)
                   U -> (x, y+1)
                   L -> (x-1, y)
                   D -> (x, y-1)
      z = 5
  in Position (x', y')
