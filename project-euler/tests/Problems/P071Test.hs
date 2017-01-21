module Problems.P071Test
  ( test_071
  ) where

import Data.Ratio
import Problems.P071

import Test.Tasty.Discover

test_071 :: [TestTree]
test_071 =
  [ testCase "main" $ solve @?= "428570"
  , testCase "non-trivial" $ fareyNeighbor 1000 (15 % 17) @?= 877 % 994
  ]
