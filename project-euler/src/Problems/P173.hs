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
 - Let L_{a,b} be the lamina where the side-length of the innermost layer is
 - `a` and the side-length of the outermost layer is `b`. Thus, the two
 - examples with 32 square tiles are L_{4,6} and L_{9,9}. Observe that L_{a,b}
 - necessarily requires that:
 -
 -   a <= b
 -   b - a is even
 -
 - The lamina with innermost sidelength `a` and `k` layers total has tilecount
 -   T(a, k) = Sum[ 4*(a + 2*i - 3) | i <- [1..k] ]
 -           = 4 * { (a-3)*k + 2*k*(k+1)/2 }
 -           = 4 * { (a-3)*k + k^2 + k }
 -           = 4k * (a + k - 2)
 -
 - For instance, L_{4,6} has tilecount T(4,2) = 4*2*(4+2-2) = 32
 -
 - Solving T(a, k) == x for k yields
 -   N[a, x] = 1/2 * (2 - a + sqrt[a^2 - 4a + x + 4]),
 -
 - So there are Floor[N[a, x]] laminae with innermost layer sidelength a that
 - can be formed with up to x tiles. Thus, we need to find
 -
 -   Sum[Floor[N[a, x]], {a, 3, x/4 + 1}]
 -
 -}
solve :: String
solve = show $ solveProblem 1e6

type SideLength = Integer
type TileCount = Integer

solveProblem :: TileCount -> Int
solveProblem maxTiles = sum $ map (layersPossible maxTiles) [3..1 + maxTiles `div` 4]

layersPossible :: TileCount -> SideLength -> Int
layersPossible x a = floor $ n (fromIntegral a) (fromIntegral x)
  where
  n a x = 0.5 * (2.0 - a + sqrt (a*a - 4.0*a + x + 4.0))
