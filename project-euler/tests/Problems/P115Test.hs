module Problems.P115Test
  ( test_115
  ) where

import Problems.P115
import Test.Tasty.Discover

test_115 :: [TestTree]
test_115 =
  [ testCase "small 3" $ solveProblem 3 @?= 30
  , testCase "small 10" $ solveProblem 10 @?= 57
  , testCase "main" $ solve @?= "168"
  ]
