module Problems.P053Test
  ( case_053_main
  ) where

import Problems.P053
import Test.Tasty.Discover (Assertion, (@?=))

case_053_main :: Assertion
case_053_main = solve @?= "233168"
