module Problems.P056Test
  ( case_056_main
  ) where

import Problems.P056
import Test.Tasty.Discover (Assertion, (@?=))

case_056_main :: Assertion
case_056_main = solve @?= "233168"
