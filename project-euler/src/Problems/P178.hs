module Problems.P178
  ( solve
  ) where

-- 178.hs
{-
 -  Consider the number 45656.
 -
 -  It can be seen that each pair of consecutive digits of 45656 has
 -  a difference of one.
 -
 -  A number for which every pair of consecutive digits has a difference of
 -  one is called a step number.
 -
 -  A pandigital number contains every decimal digit from 0 to 9 at least
 -  once.
 -
 -  How many pandigital step numbers less than 10^40 are there? 
 -}
{-
 - Given a set of step numbers which all:
 -     have <n> digits
 -     have lowest digit <l>
 -     have highest digit <h>
 -     have leftmost digit <x>
 - we can form a set of <n+1>-digit step numbers with:
 -     x' = x+1, lo' = lo, hi' = max hi (x+1)
 -     x' = x-1, lo' = min lo (x-1), hi' = hi
 - with the natural caveats that 0 <= left' <= 9
 -
 - We are looking for the set of step numbers with:
 -     <n> = 1..40
 -     <x> = 1..9
 -     <lo> = 0
 -     <hi> = 9
 -}
import Data.Array

solve :: String
solve = show $ solveProblem 40

solveProblem n = sum [pandigitalStepNumbers stp | stp <- take n step_numbers]

pandigitalStepNumbers stp = sum [stp ! (x, 0, 9) | x <- [1 .. 9]]

step_numbers = iterate takeOneStep one_digit
  where
    one_digit =
      accumArray (+) 0 ((0, 0, 0), (9, 9, 9)) [((i, i, i), 1) | i <- [0 .. 9]]

takeOneStep stp =
  let step_up =
        [ ((x + 1, l, max h (x + 1)), stp ! (x, l, h))
        | x <- [0 .. 8]
        , l <- [0 .. x]
        , h <- [x .. 9]
        ]
      step_down =
        [ ((x - 1, min l (x - 1), h), stp ! (x, l, h))
        | x <- [1 .. 9]
        , l <- [0 .. x]
        , h <- [x .. 9]
        ]
  in accumArray (+) 0 ((0, 0, 0), (9, 9, 9)) $ step_up ++ step_down
