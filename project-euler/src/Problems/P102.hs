module Problems.P102
  ( process
  ) where

{-
 - Three distinct points are plotted at random on a Cartesian plane, for which
-1000 ≤ x, y ≤ 1000, such that a triangle is formed.

Consider the following two triangles:

A(-340,495), B(-153,-910), C(835,-947)

X(-175,41), Y(-421,-714), Z(574,-645)

It can be verified that triangle ABC contains the origin, whereas triangle XYZ
does not.

Using triangles.txt (right click and 'Save Link/Target As...'), a 27K text file
containing the co-ordinates of one thousand "random" triangles, find the number
of triangles for which the interior contains the origin.

NOTE: The first two examples in the file represent the triangles in the example
given above.
 -}
type FileContents = [[Int]]

process :: FileContents -> String
process txt = show $ length $ filter containsOrigin $ parseTriangles txt

data Point =
  Point Int
        Int
  deriving (Eq, Ord, Show)

data Triangle =
  Triangle Point
           Point
           Point
  deriving (Eq, Ord, Show)

containsOrigin t@(Triangle a b c) = abs (area t - sum (map area ts)) < 1e-6
  where
    origin = Point 0 0
    ts = [Triangle a b origin, Triangle a origin c, Triangle origin b c]

area :: Triangle -> Double
area (Triangle p q r) =
  let (a, b, c) = (dist p q, dist q r, dist r p)
      s = 0.5 * (a + b + c)
  in sqrt $ s * (s - a) * (s - b) * (s - c)

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) =
  sqrt $ fromIntegral ((x2 - x1) ^ 2) + fromIntegral ((y2 - y1) ^ 2)

parseTriangles :: FileContents -> [Triangle]
parseTriangles txt =
  [ Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)
  | [x1, y1, x2, y2, x3, y3] <- txt
  ]
