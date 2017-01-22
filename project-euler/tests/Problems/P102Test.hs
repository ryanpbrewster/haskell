module Problems.P102Test
  ( test_102
  ) where

import Problems.P102
import TestData.P102 (txt)
import Test.Tasty.Discover

test_102 :: [TestTree]
test_102 =
    [ testCase "main" $ process txt @?= "228" ]
