module Problems.P493Test
  ( test_493
  ) where

import Problems.P493
import Test.Tasty.Discover

import Data.Ratio

test_493 :: [TestTree]
test_493 =
  [ testCase "main" $ solve @?= "6.818741802"
  , testCase "big" $ floor (solveProblem [1..1000] 500) @?= 368
  ]
