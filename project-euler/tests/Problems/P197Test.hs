module Problems.P197Test
  ( case_197_main
  ) where

import Problems.P197
import Test.Tasty.Discover (Assertion, (@?=))

case_197_main :: Assertion
case_197_main = solve @?= "233168"
