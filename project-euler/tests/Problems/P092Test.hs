module Problems.P092Test
  ( test_092
  ) where

import Problems.P092
import Test.Tasty.Discover

test_092 :: [TestTree]
test_092 =
  [ testCase "correctness" $ numDigitsSolve 3 @?= bruteForceSolve (10^3)
  , testCase "main" $ solve @?= "8581146"
  ]
