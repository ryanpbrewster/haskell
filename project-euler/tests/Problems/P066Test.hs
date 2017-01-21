module Problems.P066Test
  ( test_066
  ) where

import Problems.P066
import Test.Tasty.Discover

test_066 :: [TestTree]
test_066 =
  [testCase "small" $ solveProblem 7 @?= 5, testCase "main" $ solve @?= "661"]
