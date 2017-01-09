module Problems.P005Test
  ( case_005_main
  ) where

import Problems.P005
import Test.Tasty.Discover (Assertion, (@?=))

case_005_main :: Assertion
case_005_main = solve @?= "232792560"
