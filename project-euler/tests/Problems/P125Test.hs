module Problems.P125Test
  ( case_125_main
  ) where

import Problems.P125
import Test.Tasty.Discover (Assertion, (@?=))

case_125_main :: Assertion
case_125_main = solve @?= "233168"
