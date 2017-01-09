module Problems.P122Test
  ( case_122_main
  ) where

import Problems.P122
import Test.Tasty.Discover (Assertion, (@?=))

case_122_main :: Assertion
case_122_main = solve @?= "233168"
