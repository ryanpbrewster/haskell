module Problems.P173Test
  ( test_173
  ) where

import Problems.P173
import Test.Tasty.Discover

test_173 :: [TestTree]
test_173 =
  [ testCase "small" $ solveProblem 100 @?= 41
  , testCase "main" $ solve @?= "1572729"
  ]
