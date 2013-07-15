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
 -     have lowest digit <lo>
 -     have highest digit <hi>
 -     have leftmost digit <left>
 - we can form a set of <n+1>-digit step numbers with:
 -     left' = left+1, lo' = lo, hi' = max hi (left+1)
 -     left' = left-1, lo' = min lo (left-1), hi' = hi
 - with the natural caveats that 0 <= left' <= 9
 -
 - We are looking for the set of step numbers with:
 -     <n> = 40
 -     <left> = _
 -     <lo> = 0
 -     <hi> = 9
 -}

import Data.Map

main = print $ solveProblem 40

solveProblem n = sum [ pandigitalStepNumbers stp | stp <- take n step_numbers ]

pandigitalStepNumbers stp = sum [ findWithDefault 0 (left,0,9) stp | left <- [1..9]]

step_numbers = iterate takeOneStep $ fromList [((i,i,i), 1) | i <- [0..9]]

takeOneStep stp =
    let stp' = assocs stp
        step_up   = [((left+1,lo,max hi (left+1)), count) | ((left,lo,hi), count) <- stp', left < 9 ]
        step_down = [((left-1,min lo (left-1),hi), count) | ((left,lo,hi), count) <- stp', left > 0 ]
    in fromListWith (+) $ step_up ++ step_down
