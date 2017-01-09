module Problems.P147
  ( solve
  ) where

{-
 - In a 3x2 cross-hatched grid, a total of 37 different rectangles could be
 - situated within that grid as indicated in the sketch.
 -
 - There are 5 grids smaller than 3x2, vertical and horizontal dimensions being
 - important, i.e. 1x1, 2x1, 3x1, 1x2 and 2x2. If each of them is
 - cross-hatched, the following number of different rectangles could be
 - situated within those smaller grids:
 -
 - 1x1: 1 
 - 2x1: 4 
 - 3x1: 8 
 - 1x2: 4 
 - 2x2: 18
 -
 - Adding those to the 37 of the 3x2 grid, a total of 72 different rectangles
 - could be situated within 3x2 and smaller grids.
 -
 - How many different rectangles could be situated within 47x43 and smaller grids?
 -}
import qualified Data.List as DL

solve :: String
solve = show $ solveProblem 47 43

crossHatchRects wb hb = (straight_rects, corner_rects, center_rects)
  where
    straight_rects =
      [ (i, j, w, h)
      | i <- [0 .. hb - 1]
      , j <- [0 .. wb - 1]
      , let w_bound = wb - j
      , w <- [1 .. w_bound]
      , let h_bound = hb - i
      , h <- [1 .. h_bound]
      ]
    corner_rects =
      [ (i, j, w, h)
      | i <- [0 .. hb - 1]
      , j <- [1 .. wb - 1]
      , let w_bound = min (2 * wb - 2 * j) (2 * hb - 2 * i - 1)
      , w <- [1 .. w_bound]
      , let h_bound = min (2 * j) (2 * hb - 2 * i - w)
      , h <- [1 .. h_bound]
      ]
    center_rects =
      [ (i, j, w, h)
      | i <- [0.5 .. hb - 0.5]
      , j <- [0.5 .. wb - 0.5]
      , let w_bound = min (2 * wb - 2 * j) (2 * hb - 2 * i - 1)
      , w <- [1 .. w_bound]
      , let h_bound = min (2 * j) (2 * hb - 2 * i - w)
      , h <- [1 .. h_bound]
      ]

bruteForce wb hb =
  let (straight, corner, center) = crossHatchRects wb hb
  in (length straight) + (length corner) + (length center)

-- The formula was taken from the solutions thread at Project Euler
-- Ostensibly it could be derived by using the bounds I included above
-- in the brute-force solution. I do not have the patience for that.
solveProblem :: Int -> Int -> Int
solveProblem m n =
  n * (n + 1) *
  (6 * n ^ 4 - 24 * m * n ^ 3 + 30 * m ^ 2 * n ^ 2 - 6 * m * n ^ 2 - 15 * n ^ 2 +
   5 * m ^ 3 * n +
   45 * m ^ 2 * n +
   46 * m * n +
   15 * n +
   10 * m ^ 3 +
   15 * m ^ 2 -
   31 * m -
   6) `div`
  180
