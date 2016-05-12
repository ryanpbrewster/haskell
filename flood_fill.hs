-- imperative_programming_2.hs
{-
 - Flood-fill using the ST monad (and a reference "pure" non-monadic implementation).
 -}

import Control.Monad.ST.Lazy
import Debug.Trace
import Data.Array.ST
import qualified Data.Array as A

c_GRID0 = [ "....xxxxxxxxxxxx"
          , "x....xx.......xx"
          , "xxxx....xxxx..xx"
          , "x......xx.....xx"
          , "xxxxxxxxx..xxxxx"
          ]
c_GRID1 = let n = 100 in [[ if (i*n+j) `mod` 97 == 0 && j > 0 then 'x' else '.' | j <- [0..n-1]] | i <- [0..n-1]]
main = do
  let grid = toGrid c_GRID1
  print $ length $ explorePure grid

type Position = (Int, Int)
toGrid :: [[Char]] -> A.Array Position Bool
toGrid charGrid =
  let (m, n) = (length charGrid, length $ head charGrid)
  in A.listArray ((0, 0), (m-1, n-1)) (map (=='x') $ concat charGrid)

neighbors :: Position -> [Position]
neighbors (i, j) = [ (i,j+1), (i-1,j), (i,j-1), (i+1,j) ]

explorePure :: A.Array Position Bool -> [Position]
explorePure grid0 = explore' (grid0 A.// [((0,0), True)]) [(0,0)]
  where
  (m, n) = let ((0, 0), (ihi, jhi)) = A.bounds grid0 in (ihi + 1, jhi + 1)
  inBounds (i, j) = 0 <= i && i < m && 0 <= j && j < n
  explore' grid [] = []
  explore' grid (p:ps) =
    let ns = filter (\n -> inBounds n && not (grid A.! n)) (neighbors p)
        ps' = ns ++ ps
        grid' = grid A.// zip ns (repeat True)
    in p : explore' grid' ps'

exploreST :: A.Array Position Bool -> [Position]
exploreST initGrid = runST $ do
  visGrid <- newArray (A.bounds initGrid) False
  explore' visGrid [(0,0)]
  where
  (m, n) = let ((0, 0), (ihi, jhi)) = A.bounds initGrid in (ihi + 1, jhi + 1)
  inBounds (i, j) = 0 <= i && i < m && 0 <= j && j < n
  explore' :: STArray s Position Bool -> [Position] -> ST s [Position]
  explore' visGrid [] = return []
  explore' visGrid (p:ps) = do
    vis <- readArray visGrid p
    if vis || initGrid A.! p then explore' visGrid ps else do
      writeArray visGrid p True
      rest <- explore' visGrid (filter inBounds (neighbors p) ++ ps)
      return (p : rest)
