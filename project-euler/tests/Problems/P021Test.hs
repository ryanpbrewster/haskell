module Problems.P021Test
  ( case_021_main
  ) where

import Problems.P021
import Test.Tasty.Discover (Assertion, (@?=))

case_021_main :: Assertion
case_021_main = solve @?= "31626"
