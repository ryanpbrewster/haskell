module Problems.P076Test
  ( case_076_main
  ) where

import Problems.P076
import Test.Tasty.Discover (Assertion, (@?=))

case_076_main :: Assertion
case_076_main = solve @?= "233168"
