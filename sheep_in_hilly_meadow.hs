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

import qualified Data.Array as A
import qualified Data.Set as S

main = do
  grid <- fmap parseGrid getContents
  print $ findMinK grid

parseGrid :: String -> (A.Array (Int, Int) Int)
parseGrid txt =
  let txtGrid = map words $ lines txt
      (m, n) = (length txtGrid, length $ head txtGrid)
  in A.listArray ((0,0),(m-1,n-1)) (map read $ concat txtGrid)

findMinK :: A.Array (Int, Int) Int -> Int
findMinK grid =
  let ((ilo, jlo), (ihi, jhi)) = A.bounds grid
      area = (ihi - ilo + 1) * (jhi - jlo + 1)
      validKs = filter (\k -> numAccessibleCells grid k >= area `div` 2 ) [0..]
  in head validKs

numAccessibleCells :: A.Array (Int, Int) Int -> Int -> Int
numAccessibleCells grid k = S.size $ accessibleCells [(0,0)] (S.empty)
  where
  inBounds (i,j) =
    let ((ilo, jlo), (ihi, jhi)) = A.bounds grid
    in ilo <= i && i <= ihi && jlo <= j && j <= jhi
  neighbors (i,j) = [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
  accessibleCells [] visited = visited
  accessibleCells (n:q) visited
    | not (inBounds n) || grid A.! n > k || S.member n visited = accessibleCells q visited
    | otherwise = accessibleCells (q ++ neighbors n) (S.insert n visited)
