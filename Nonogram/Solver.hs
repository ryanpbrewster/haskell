module Nonogram.Solver
where

import Nonogram.Base
import qualified Data.Array as A
import Control.Monad (replicateM)

solve :: Constraints -> Solution
solve = bruteforceSolver

bruteforceSolver constraints =
  let candidates = enumerateAll $ dimensions constraints
      sols = filter (`isConsistentWith` constraints) candidates
  in head sols


-- Produce all possible solutions with given dimensions
enumerateAll :: (Int, Int) -> [Solution]
enumerateAll (nr,nc) =
  let enumerated_elements = replicateM (nr*nc) [White, Black]
  in [ Solution $ A.listArray ((1,1), (nr,nc)) elements | elements <- enumerated_elements ]

isConsistentWith :: Solution -> Constraints -> Bool
solution `isConsistentWith` constraints = 
  generateConstraints solution == constraints
