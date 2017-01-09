module Problems.P108Test
  ( case_108_main
  ) where

import Problems.P108
import Test.Tasty.Discover (Assertion, (@?=))

case_108_main :: Assertion
case_108_main = solve @?= "233168"
