module Problems.P114Test
  ( test_114
  ) where

import Problems.P114
import Test.Tasty.Discover

test_114 :: [TestTree]
test_114 =
  [ testCase "small" $ ways !! 7 @?= 17
  , testCase "main" $ solve @?= "16475640049"
  ]
