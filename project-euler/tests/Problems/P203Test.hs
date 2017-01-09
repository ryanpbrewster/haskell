module Problems.P203Test
  ( case_203_main
  ) where

import Problems.P203
import Test.Tasty.Discover (Assertion, (@?=))

case_203_main :: Assertion
case_203_main = solve @?= "233168"
