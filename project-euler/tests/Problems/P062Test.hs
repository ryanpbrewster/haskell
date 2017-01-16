module Problems.P062Test
  ( test_062
  ) where

import Problems.P062
import Test.Tasty.Discover

test_062 :: [TestTree]
test_062 =
    [ testCase "main" $ solve @?= "127035954683"
    , testCase "big" $ solveProblem 8 @?= 12345
    ]
