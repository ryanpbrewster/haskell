-- 091.hs
{-
 - The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and
 - are joined to the origin, O (0,0), to form ΔOPQ.
 -
 -
 - There are exactly fourteen triangles containing a right angle that can be
 - formed when each co-ordinate lies between 0 and 2 inclusive; that is, 0  x1,
 - y1, x2, y2  2.
 -
 -
 - Given that 0 <= x1, y1, x2, y2 <= 50, how many right triangles can be formed?
 -}

isRightTriangle x1 y1 x2 y2 x3 y3 =
    let d12 = (x2-x1)^2 + (y2-y1)^2
        d13 = (x3-x1)^2 + (y3-y1)^2
        d23 = (x3-x2)^2 + (y3-y2)^2
    in d12 + d13 == d23 || d12 + d23 == d13 || d13 + d23 == d12

-- The x1*y2 > x2*y1 thing is to avoid double-counting
-- We simply require that Q (at x2,y2) make a larger angle than P (at x1,y1)
-- Thus, theta2 > theta1
--  -->  Tan[theta2] > Tan[theta1]
--  -->  y2/x2 > y1/x1
--  --> x1*y2 > x2*y1
solveProblem bound =
    length $ [ [(0,0), (x1,y1), (x2,y2)] | x1 <- [0..bound]
                                         , y1 <- [0..bound]
                                         , x2 <- [0..bound]
                                         , y2 <- [0..bound]
                                         , x1*y2 > x2*y1
                                         , isRightTriangle 0 0 x1 y1 x2 y2 ]

main = print $ solveProblem 50
