module Problems.P114Test
  ( case_114_main
  ) where

import Problems.P114
import Test.Tasty.Discover (Assertion, (@?=))

case_114_main :: Assertion
case_114_main = solve @?= "233168"
