module Problems.P129Test
  ( case_129_main
  ) where

import Problems.P129
import Test.Tasty.Discover (Assertion, (@?=))

case_129_main :: Assertion
case_129_main = solve @?= "233168"
