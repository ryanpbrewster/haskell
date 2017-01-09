module Problems.P023Test
  ( case_023_main
  ) where

import Problems.P023
import Test.Tasty.Discover (Assertion, (@?=))

case_023_main :: Assertion
case_023_main = solve @?= "4179871"
