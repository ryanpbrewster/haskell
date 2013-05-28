-- 085.hs
{-
 - By counting carefully it can be seen that a rectangular grid measuring 2 by
 - 3 contains eighteen rectangles:
 -
 -     6            4          2
 - +--+--+--+  +--+--+--+  +--+--+--+
 - |XX|  |  |  |XXXXX|  |  |XXXXXXXX|
 - +--|--+--+  +--|--+--+  +--|--+--+
 - |  |  |  |  |  |  |  |  |  |  |  |
 - +--+--+--+  +--+--+--+  +--+--+--+
 -
 -     3            2          1
 - +--+--+--+  +--+--+--+  +--+--+--+
 - |XX|  |  |  |XXXXX|  |  |XXXXXXXX|
 - |XX|--+--+  |XXXXX|--+  |XXXXXXXX|
 - |XX|  |  |  |XXXXX|  |  |XXXXXXXX|
 - +--+--+--+  +--+--+--+  +--+--+--+
 -
 - Although there exists no rectangular grid that contains exactly two million
 - rectangles, find the area of the grid with the nearest solution.
 -}

{-
 - In general, suppose we have a big rectangle of size (wb,hb).
 - If we want to put a smaller rectangle with upper-left corner at (x,y) then it
 - can have width w = [1..wb-x] and height h = [1..hb-h]. Since we can place a rectangle
 - at any point x = [0..wb-1] and y = [0..hb-1] we have
 -     Sum[1, {x,0,wb-w}, {y,0,hb-h}, {w,1,wb-x}, {h,1,hb-y}] == wb*(wb+1)*hb*(hb+1)/4
 -
 -
 - Now, the problem reduces to finding the point (w,h) with w,h >= 1
 - has `rects w h` as close to `target` as possible.
 -
 - Most of this code is for that general type of problem. Given a monotonically
 - increasing function `f`, find the lattice point where `f x y` is as close as
 - possible to a given value.
 -}

import Data.List (minimumBy)
import Data.Ord (comparing)

rects wb hb = wb*(wb+1)*hb*(hb+1) `div` 4

main = print $ solveProblem (2*10^6)

solveProblem targ = let pts = goodLatticePoints (uncurry rects) targ
                        errs = map (abs . (subtract targ) . (uncurry rects)) pts
                        best = minimumBy (comparing snd) $ zip pts errs
                        (bx,by) = fst best
                    in bx*by

-- f, by assumption, is a monotonically non-decreasing function of two variables
-- and we're looking for points where f x y \approx targ
goodLatticePoints f targ =
    let x_points = [ (x,1) | x <- [1..] ]
        x_cross = findCrossing f x_points targ
        cross_points = concat $ iterate (nextCrossing f targ) x_cross
    in takeWhile (\(x,y) -> x > 0) cross_points

nextCrossing f targ [(x1,y1), (x2,y2)] =
    let y = y1
        xr = max x1 x2
    in findCrossing f [(x,y+1) | x <- [xr,xr-1..]] targ

-- findCrossing takes in a function, a list of points, and a target it finds
-- the first pair of points that straddles a crossing-point
-- That is, it returns two points, p1 and p2
-- It is guaranteed that in moving from p1 to p2 we will cross some point where
--     f pt == targ
findCrossing f pts targ =
    let vals = [ (f pt) - targ | pt <- pts ]
        f0 = head vals
        pt_vals = zip pts vals
        (l,r) = span (\(p,v) -> (v<0) == (f0<0)) pt_vals -- split at the cross
        pt_l = fst $ last l
        pt_r = fst $ head r
    in [pt_l, pt_r]
