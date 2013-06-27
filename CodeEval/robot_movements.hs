-- robot_movements.hs
{-
 - A robot is located at the top-left corner of a 4x4 grid. The robot can move
 - either up, down, left, or right, but can not visit the same spot twice. The
 - robot is trying to reach the bottom-right corner of the grid.
 - Input sample:
 - 
 - There is no input for this program.
 - Output sample:
 - 
 - Print out the unique number of ways the robot can reach its destination. (The
 - number should be printed as an integer whole number eg. if the answer is 10
 - (its not !!), print out 10, not 10.0 or 10.00 etc)
 -}

data Point = Point Int Int deriving (Show, Eq)

neighbors4 (Point x y) = [ Point (x+1) y
                         , Point x (y+1)
                         , Point (x-1) y
                         , Point x (y-1)
                         ]

inBounds (Point x_lo y_lo) (Point x_hi y_hi) (Point x y) =
    x_lo <= x && x <= x_hi && y_lo <= y && y <= y_hi

ways path@(cur:_) target | cur == target = 1
                         | otherwise =
    let legit p = inBounds (Point 0 0) target p && not (p `elem` path)
        moves = filter legit $ neighbors4 cur
    in sum [ ways (next:path) target | next <- moves ]


main = print $ ways [(Point 0 0)] (Point 3 3)
