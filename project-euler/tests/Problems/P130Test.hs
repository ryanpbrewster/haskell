module Problems.P130Test
  ( case_130_main
  ) where

import Problems.P130
import Test.Tasty.Discover (Assertion, (@?=))

case_130_main :: Assertion
case_130_main = solve @?= "233168"
