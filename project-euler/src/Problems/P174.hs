module Problems.P174
  ( solve
  ) where

{-
 - We shall define a square lamina to be a square outline with a square "hole"
 - so that the shape possesses vertical and horizontal symmetry.
 -
 - Given eight tiles it is possible to form a lamina in only one way: 3x3
 - square with a 1x1 hole in the middle. However, using thirty-two tiles it is
 - possible to form two distinct laminae.
 -
 -
 - If t represents the number of tiles used, we shall say that t = 8 is type
 - L(1) and t = 32 is type L(2).
 -
 - Let N(n) be the number of t ≤ 1000000 such that t is type L(n); for example,
 - N(15) = 832.
 -
 - What is ∑ N(n) for 1 ≤ n ≤ 10?
 -}

import qualified Data.Array as A
import qualified Data.Map as M

solve :: String
solve = show $ sum $ map numLaminae [1..10]

bound = 1e6

numLaminae n = M.findWithDefault 0 n tally
  where
  waysToForm = A.accumArray (+) 0 (1, bound) $ zip allLaminae (repeat 1)
  tally = M.fromListWith (+) $ zip (A.elems waysToForm) (repeat 1)

allLaminae = [ 4 * k * (k + a) | k <- layers, a <- holes k ]
  where
    layers = takeWhile (\k -> area 1 k <= bound) [1..]
    holes k = takeWhile (\a -> area a k <= bound) [1..]

area a k = 4 * k * (k + a)
