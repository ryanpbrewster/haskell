module Problems.P206Test
  ( case_206_main
  ) where

import Problems.P206
import Test.Tasty.Discover (Assertion, (@?=))

case_206_main :: Assertion
case_206_main = solve @?= "233168"
