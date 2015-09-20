module Nonogram.Tests
where

import Nonogram.Base

tests = [ Constraints [Constraint (0,2), Constraint (2,0)] [Constraint (1,1), Constraint(1,1)]
        , Constraints (replicate 2 (Constraint (1,1))) (replicate 2 (Constraint (1,1)))
        , Constraints (replicate 3 (Constraint (2,1))) (replicate 3 (Constraint (2,1)))
        , Constraints (replicate 4 (Constraint (3,1))) (replicate 4 (Constraint (3,1)))
        ]
