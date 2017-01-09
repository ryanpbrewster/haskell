module Problems.P116
  ( solve
  ) where

{-
 - A row of five black square tiles is to have a number of its tiles replaced
 - with coloured oblong tiles chosen from red (length two), green (length
 - three), or blue (length four).
 -
 - If red tiles are chosen there are exactly seven ways this can be done.
 -     rr...
 -     .rr..
 -     ..rr.
 -     ...rr
 -     rrrr.
 -     rr.rr
 -     .rrrr
 -
 - If green tiles are chosen there are three ways.
 -     ggg..
 -     .ggg.
 -     ..ggg
 -
 - And if blue tiles are chosen there are two ways.
 -     bbbb.
 -     .bbbb
 -
 - Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
 - replacing the black tiles in a row measuring five units in length.
 -
 - How many different ways can the black tiles in a row measuring fifty units
 - in length be replaced if colours cannot be mixed and at least one coloured
 - tile must be used?  A row of five black square tiles is to have a number of
 - its tiles replaced with coloured oblong tiles chosen from red (length two),
 - green (length three), or blue (length four).
 -}
{-
 - This is basically the same as 114
 - For red, you can either add a black tile or two red tiles,
 - which corresponds to r[n-1] or r[n-2]
 -     r[n] = r[n-1] + r[n-2]
 -     g[n] = g[n-1] + g[n-3]
 -     b[n] = b[n-1] + b[n-4]
 -}
solve :: String
solve = show $ solveProblem 50

r = [1, 1] ++ zipWith (+) (r) (drop 1 r)

g = [1, 1, 1] ++ zipWith (+) (g) (drop 2 g)

b = [1, 1, 1, 1] ++ zipWith (+) (b) (drop 3 b)

add3 x y z = x + y + z

f = map (subtract 3) $ zipWith3 add3 r g b

solveProblem n = f !! n
