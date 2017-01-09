module Problems.P035Test
  ( test_035
  ) where

import Problems.P035
import Test.Tasty.Discover

test_035 :: [TestTree]
test_035 =
  [ testCase "rotations" $ rotations 123 @?= [123, 312, 231]
  , testCase "main" $ solve @?= "55"
  ]
