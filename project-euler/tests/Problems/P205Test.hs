module Problems.P205Test
  ( case_205_main
  ) where

import Problems.P205
import Test.Tasty.Discover (Assertion, (@?=))

case_205_main :: Assertion
case_205_main = solve @?= "233168"
