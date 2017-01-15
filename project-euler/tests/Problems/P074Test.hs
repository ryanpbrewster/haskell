module Problems.P074Test
  ( test_074
  ) where

import Problems.P074
import Test.Tasty.Discover

test_074 :: [TestTree]
test_074 =
  [ testCase "main" $ solve @?= "402"
  ]
