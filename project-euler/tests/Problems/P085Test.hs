module Problems.P085Test
  ( case_085_main
  ) where

import Problems.P085
import Test.Tasty.Discover (Assertion, (@?=))

case_085_main :: Assertion
case_085_main = solve @?= "233168"
