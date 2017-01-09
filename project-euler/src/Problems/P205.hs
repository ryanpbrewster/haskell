module Problems.P205
  ( solve
  ) where

{-
 - Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2,
 - 3, 4.  Colin has six six-sided (cubic) dice, each with faces numbered 1, 2,
 - 3, 4, 5, 6.
 -
 - Peter and Colin roll their dice and compare totals: the highest total wins.
 - The result is a draw if the totals are equal.
 -
 - What is the probability that Pyramidal Pete beats Cubic Colin? Give your
 - answer rounded to seven decimal places in the form 0.abcdefg
 -}
import Data.Array
import Data.Ratio
import Text.Printf (printf)

-- diceRolls dice
diceRolls [] = [(0, 1)]
diceRolls (die:dice) =
  let rolls = diceRolls dice
      rolls' = [(v_r + v, ways_r) | v <- [1 .. die], (v_r, ways_r) <- rolls]
  in assocs $ accumArray (+) 0 (0, die + sum dice) rolls'

winningProbability p1 p2 =
  let p2_cum = scanl (+) 0 p2
      win_probs = zipWith (*) p1 p2_cum
  in (sum win_probs) % ((sum p1) * (sum p2))

solveProblem =
  let pete = diceRolls $ replicate 9 4
      colin = diceRolls $ replicate 6 6
      highest_roll = maximum [v | (v, ways) <- pete ++ colin]
      pete' = accumArray (+) 0 (0, highest_roll) pete
      colin' = accumArray (+) 0 (0, highest_roll) colin
  in winningProbability (elems pete') (elems colin')

solve :: String
solve = printf "%.7f\n" (fromRational solveProblem :: Double)
