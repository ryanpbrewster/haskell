module Problems.P124Test
  ( case_124_main
  ) where

import Problems.P124
import Test.Tasty.Discover (Assertion, (@?=))

case_124_main :: Assertion
case_124_main = solve @?= "233168"
