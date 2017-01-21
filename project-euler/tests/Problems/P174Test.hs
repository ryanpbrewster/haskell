module Problems.P174Test
  ( test_174
  ) where

import Problems.P174
import Test.Tasty.Discover

test_174 :: [TestTree]
test_174 = [testCase "main" $ solve @?= "209566"]
