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

import qualified Data.Array as A
import qualified Data.IntSet as S
import qualified Data.PSQueue as PQ

type Height = Int
type Bravery = Int
type Position = (Int, Int)

main = do
  grid <- fmap parseGrid getContents
  print $ findMinK grid 0.5

parseGrid :: String -> (A.Array Position Height)
parseGrid txt =
  let txtGrid = map words $ lines txt
      (m, n) = (length txtGrid, length $ head txtGrid)
  in A.listArray ((0,0),(m-1,n-1)) (map read $ concat txtGrid)

findMinK :: A.Array Position Height -> Double -> Bravery
findMinK grid chi =
  let ((ilo, jlo), (ihi, jhi)) = A.bounds grid
      area = (ihi - ilo + 1) * (jhi - jlo + 1)
      threshold = ceiling (chi * fromIntegral area)
  in fst $ (explore grid) !! threshold

explore :: A.Array Position Height -> [(Bravery, Position)]
explore grid = exploreHelper (PQ.singleton (0, 0) 0) (S.singleton 0)
  where
  exploreHelper (PQ.minView -> Nothing) vis = []
  exploreHelper (PQ.minView -> Just (pos PQ.:-> k, pq)) vis =
    let newPositions = filter (\n -> inBounds n && not (S.member (flat n) vis)) (neighbors pos)
        updatePQ pq_ pos_ = PQ.insert pos_ (max k (grid A.! pos_)) pq_
        pq' = foldl updatePQ pq newPositions
    in (k, pos) : (exploreHelper pq' (S.union vis $ S.fromList $ map flat newPositions))

  (m, n) = let ((0,0), (ihi,jhi)) = A.bounds grid in (ihi+1, jhi+1)
  flat (i, j) = n * i + j
  inBounds (i,j) = 0 <= i && i < m && 0 <= j && j < n

neighbors (i,j) = [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
