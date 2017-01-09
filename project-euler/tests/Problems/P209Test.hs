module Problems.P209Test
  ( case_209_main
  ) where

import Problems.P209
import Test.Tasty.Discover (Assertion, (@?=))

case_209_main :: Assertion
case_209_main = solve @?= "233168"
