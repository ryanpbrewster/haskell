module Problems.P048Test
  ( case_048_main
  ) where

import Problems.P048
import Test.Tasty.Discover (Assertion, (@?=))

case_048_main :: Assertion
case_048_main = solve @?= "9110846700"
