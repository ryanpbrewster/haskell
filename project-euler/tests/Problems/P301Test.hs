module Problems.P301Test
  ( case_301_main
  ) where

import Problems.P301
import Test.Tasty.Discover (Assertion, (@?=))

case_301_main :: Assertion
case_301_main = solve @?= "233168"
