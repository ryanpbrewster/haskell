module Problems.P117Test
  ( case_117_main
  ) where

import Problems.P117
import Test.Tasty.Discover (Assertion, (@?=))

case_117_main :: Assertion
case_117_main = solve @?= "100808458960497"
