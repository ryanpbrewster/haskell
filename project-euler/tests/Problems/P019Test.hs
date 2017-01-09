module Problems.P019Test
  ( case_019_main
  ) where

import Problems.P019
import Test.Tasty.Discover (Assertion, (@?=))

case_019_main :: Assertion
case_019_main = solve @?= "171"
