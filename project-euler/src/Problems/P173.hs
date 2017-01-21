module Problems.P173
  ( solve
  , solveProblem
  ) where

{-
 - We shall define a square lamina to be a square outline with a square "hole"
 - so that the shape possesses vertical and horizontal symmetry. For example,
 - using exactly thirty-two square tiles we can form two different square
 - laminae:
 -
 -   xxxxxx    xxxxxxxxx
 -   xxxxxx    x       x
 -   xx  xx    x       x
 -   xx  xx    x       x
 -   xxxxxx    x       x
 -   xxxxxx    x       x
 -             x       x
 -             x       x
 -             xxxxxxxxx
 -
 - With one-hundred tiles, and not necessarily using all of the tiles at one
 - time, it is possible to form forty-one different square laminae.
 -
 - Using up to one million tiles how many different square laminae can be
 - formed?
 -}
{-
 - Let L_{a,b} be the lamina where the side-length of the "hole"
 - `a` and the side-length of the outermost layer is `b`. Thus, the two
 - examples with 32 square tiles are L_{2,6} and L_{7,9}. Observe that L_{a,b}
 - necessarily requires that:
 -
 -   a < b
 -   b - a is even
 -
 - The lamina with "hole" sidelength `a` and total layers `k` has tilecount
 -   T(a, k) = (a+2*k)^2 - a^2 = 4*k*(k+a)
 -
 - For instance, L_{2,6} has tilecount T(2,2) = 4*2*4 = 32 tiles, as expected.
 -
 - How many laminae with exactly `k` layers exist with T(a, k) <= x?
 -   T(a, k) = 4*k*(k+a) <= x --> a <= x/(4k) - k
 -
 - We can put a bound on k, because a >= 1 and T(1, k) <= x
 -  --> 4*k*(k+1) <= x --> 4*k^2 < x
 -
 -}
solve :: String
solve = show $ solveProblem 1e6

solveProblem maxTiles =
  sum $
  map
    (layersPossible maxTiles)
    (takeWhile (\k -> 4 * k * (k + 1) <= maxTiles) [1 ..])

layersPossible x k = x `div` (4 * k) - k
