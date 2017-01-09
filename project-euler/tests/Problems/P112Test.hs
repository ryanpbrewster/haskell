module Problems.P112Test
  ( case_112_main
  ) where

import Problems.P112
import Test.Tasty.Discover (Assertion, (@?=))

case_112_main :: Assertion
case_112_main = solve @?= "233168"
