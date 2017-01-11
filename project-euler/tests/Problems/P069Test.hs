module Problems.P069Test
  ( test_069
  ) where

import Problems.P069
import Test.Tasty.Discover

test_069 =
  [ testCase "main" $ solve @?= "510510"
  , testCase "10^9" $ solveProblem (10 ^ 9) @?= 223092870
  ]
