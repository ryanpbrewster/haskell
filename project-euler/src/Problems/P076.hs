module Problems.P076 (solve) where

{-
 - It is possible to write five as a sum in exactly six different ways:
 -
 - 4 + 1
 - 3 + 2
 - 3 + 1 + 1
 - 2 + 2 + 1
 - 2 + 1 + 1 + 1
 - 1 + 1 + 1 + 1 + 1
 -
 - How many different ways can one hundred be written as a sum of at least two positive integers?
 -}

import Util.Math (coinCombos)

solve :: String
solve = show $ (coinCombos [1..99]) !! 100
