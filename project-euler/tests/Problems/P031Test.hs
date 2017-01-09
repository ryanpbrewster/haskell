module Problems.P031Test
  ( case_031_main
  ) where

import Problems.P031
import Test.Tasty.Discover (Assertion, (@?=))

case_031_main :: Assertion
case_031_main = solve @?= "73682"
