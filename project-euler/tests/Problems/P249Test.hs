module Problems.P249Test
  ( case_249_main
  ) where

import Problems.P249
import Test.Tasty.Discover (Assertion, (@?=))

case_249_main :: Assertion
case_249_main = solve @?= "233168"
