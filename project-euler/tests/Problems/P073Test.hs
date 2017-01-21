module Problems.P073Test
  ( test_073
  ) where

import Problems.P073
import Test.Tasty.Discover

test_073 :: [TestTree]
test_073 = [testCase "main" $ solve @?= "7295372"]
