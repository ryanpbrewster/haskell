module Problems.P028Test
  ( case_028_main
  ) where

import Problems.P028
import Test.Tasty.Discover (Assertion, (@?=))

case_028_main :: Assertion
case_028_main = solve @?= "669171001"
