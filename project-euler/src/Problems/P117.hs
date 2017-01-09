module Problems.P117
  ( solve
  ) where

{-
 - Using a combination of black square tiles and oblong tiles chosen from: red
 - tiles measuring two units, green tiles measuring three units, and blue tiles
 - measuring four units, it is possible to tile a row measuring five units in
 - length in exactly fifteen different ways.
 - 
 -     .....
 -     rr...
 -     .rr..
 -     ..rr.
 -     ...rr
 -     ggg..
 -     .ggg.
 -     ..ggg
 -     bbbb.
 -     .bbbb
 -     rrggg
 -     gggrr
 - 
 - How many ways can a row measuring fifty units in length be tiled?
 - 
 - NOTE: This is related to problem 116.
 -}
{-
 - They weren't kidding about it being related to 116.
 - This is basically the same problem. Just now we have to tie it all together
 - ways(n) = use_black + use_red + use_green + use_blue
 - w[n] = w[n-1] + w[n-2] + w[n-3] + w[n-4]
 -}
import Data.List (zipWith4)

solve :: String
solve = show $ solveProblem 50

solveProblem n = f !! n

add4 w x y z = w + x + y + z

-- f[n] ==                      f[n-4] +   f[n-3] +   f[n-2] +   f[n-1]
f = [1, 1, 2, 4] ++ zipWith4 add4 (drop 0 f) (drop 1 f) (drop 2 f) (drop 3 f)
