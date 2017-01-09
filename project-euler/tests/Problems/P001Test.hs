module Problems.P001Test
  ( case_001_main
  ) where

import Problems.P001
import Test.Tasty.Discover (Assertion, (@?=))

case_001_main :: Assertion
case_001_main = solve @?= "233168"
