module Problems.P317Test
  ( case_317_main
  ) where

import Problems.P317
import Test.Tasty.Discover (Assertion, (@?=))

case_317_main :: Assertion
case_317_main = solve @?= "233168"
