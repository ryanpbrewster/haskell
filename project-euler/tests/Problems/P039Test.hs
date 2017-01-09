module Problems.P039Test
  ( case_039_main
  ) where

import Problems.P039
import Test.Tasty.Discover (Assertion, (@?=))

case_039_main :: Assertion
case_039_main = solve @?= "840"
