module Problems.P240Test
  ( case_240_main
  ) where

import Problems.P240
import Test.Tasty.Discover (Assertion, (@?=))

case_240_main :: Assertion
case_240_main = solve @?= "233168"
