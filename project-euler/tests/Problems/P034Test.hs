module Problems.P034Test
  ( case_034_main
  ) where

import Problems.P034
import Test.Tasty.Discover (Assertion, (@?=))

case_034_main :: Assertion
case_034_main = solve @?= "40730"
