module Problems.P012Test
  ( case_012_main
  ) where

import Problems.P012
import Test.Tasty.Discover (Assertion, (@?=))

case_012_main :: Assertion
case_012_main = solve @?= "76576500"
