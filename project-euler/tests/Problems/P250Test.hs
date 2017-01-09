module Problems.P250Test
  ( case_250_main
  ) where

import Problems.P250
import Test.Tasty.Discover (Assertion, (@?=))

case_250_main :: Assertion
case_250_main = solve @?= "233168"
