-- sheep_in_hilly_meadow.hs
{-
Consider a rectangular meadow divided into a grid, each cell with a height

+---+---+---+
| 0 | 7 | 9 |
+---+---+---+
| 4 | 2 | 5 |
+---+---+---+

a bunch of sheep are in the upper-left, and they refuse to go into a
cell with value higher than K, some parameter.

Find the smallest value of K such that the sheep can access more than half
of the meadow (sheep can only move up/down/left/right
-}

{-# LANGUAGE ViewPatterns #-}

import Debug.Trace (traceShow)
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Array as A
import qualified Data.PQueue.Min as PQ
import qualified Data.Array.ST as AST
import qualified Control.Monad.ST.Lazy as ST

type Height = Int
type Bravery = Int
type Position = (Int, Int)

main = do
  grid <- fmap parseGrid (BS.readFile "sheep_in_hilly_meadow.in.1000x1000")
  print $ findMinK grid 0.5

parseGrid :: BS.ByteString -> (A.Array Position Height)
parseGrid txt =
  let (m:n:grid) = map (fst . fromJust . BS.readInt) (BS.words txt)
  in A.listArray ((0,0),(m-1,n-1)) grid

findMinK :: A.Array Position Height -> Double -> Bravery
findMinK grid chi =
  let ((0, 0), (ihi, jhi)) = A.bounds grid
      area = (ihi - 1) * (jhi + 1)
      threshold = ceiling (chi * fromIntegral area)
  in fst $ (explore grid) !! threshold

explore :: A.Array Position Height -> [(Bravery, Position)]
explore grid = ST.runST $ do
  visGrid <- AST.newArray (A.bounds grid) False :: ST.ST s (AST.STArray s Position Bool)
  AST.writeArray visGrid (0,0) True
  explore' (PQ.singleton (0, (0, 0))) visGrid
  where
  explore' :: PQ.MinQueue (Height, Position) -> AST.STArray s Position Bool -> ST.ST s [(Height, Position)]
  explore' (PQ.minView -> Nothing) visGrid = return []
  explore' (PQ.minView -> Just ((k, pos), pq)) visGrid = do
    AST.writeArray visGrid pos True
    let candidates = filter inBounds (neighbors pos)
    candVis <- mapM (AST.readArray visGrid) candidates
    let newPositions = [ n | (n, v) <- zip candidates candVis, not v ]
    let pq' = foldl (updatePQ k) pq newPositions
    forM_ newPositions $ \n -> AST.writeArray visGrid n True
    rest <- explore' pq' visGrid
    return ((k, pos) : rest)

  (m, n) = let ((0,0), (ihi,jhi)) = A.bounds grid in (ihi+1, jhi+1)
  inBounds (i,j) = 0 <= i && i < m && 0 <= j && j < n
  updatePQ k pq pos = let prio = max k (grid A.! pos) in PQ.insert (prio, pos) pq

neighbors (i,j) = [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
