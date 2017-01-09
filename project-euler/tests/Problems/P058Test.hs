module Problems.P058Test
  ( case_058_main
  ) where

import Problems.P058
import Test.Tasty.Discover (Assertion, (@?=))

case_058_main :: Assertion
case_058_main = solve @?= "233168"
