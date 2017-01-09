module Problems.P132Test
  ( case_132_main
  ) where

import Problems.P132
import Test.Tasty.Discover (Assertion, (@?=))

case_132_main :: Assertion
case_132_main = solve @?= "233168"
