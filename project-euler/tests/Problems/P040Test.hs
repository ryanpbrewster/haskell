module Problems.P040Test
  ( case_040_main
  ) where

import Problems.P040
import Test.Tasty.Discover (Assertion, (@?=))

case_040_main :: Assertion
case_040_main = solve @?= "210"
