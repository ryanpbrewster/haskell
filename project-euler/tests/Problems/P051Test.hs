module Problems.P051Test
  ( case_051_main
  ) where

import Problems.P051
import Test.Tasty.Discover (Assertion, (@?=))

case_051_main :: Assertion
case_051_main = solve @?= "121313"
