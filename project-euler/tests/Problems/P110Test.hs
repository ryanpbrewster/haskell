module Problems.P110Test
  ( case_110_main
  ) where

import Problems.P110
import Test.Tasty.Discover (Assertion, (@?=))

case_110_main :: Assertion
case_110_main = solve @?= "9350130049860600"
