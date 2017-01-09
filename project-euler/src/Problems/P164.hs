module Problems.P164
  ( solve
  ) where

-- 164.hs
{-
 - How many 20 digit numbers n (without any leading zero) exist such that no
 - three consecutive digits of n have a sum greater than 9?
 -}
import Data.Array

solve :: String
solve = show $ g 20

bound = 9

f d = sum [f' (d - 1) i i | i <- [1 .. 9]]
  where
    f' 0 _ _ = 1
    f' d one two = sum [f' (d - 1) i (one + i) | i <- [0 .. bound - two]]

g d =
  let arr = g' (d - 1)
  in sum [arr ! (i, i) | i <- [1 .. 9]]

g' 0 = listArray ((0, 0), (bound, bound)) (repeat 1)
g' d =
  let arr = g' (d - 1)
  in array
       ((0, 0), (bound, bound))
       [ ((one, two), ans)
       | one <- [0 .. bound]
       , two <- [one .. bound]
       , let ans = sum [arr ! (i, one + i) | i <- [0 .. bound - two]]
       ]
