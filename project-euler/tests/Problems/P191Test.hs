module Problems.P191Test
  ( case_191_main
  ) where

import Problems.P191
import Test.Tasty.Discover (Assertion, (@?=))

case_191_main :: Assertion
case_191_main = solve @?= "233168"
