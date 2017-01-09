module Problems.P043Test
  ( case_043_main
  ) where

import Problems.P043
import Test.Tasty.Discover (Assertion, (@?=))

case_043_main :: Assertion
case_043_main = solve @?= "16695334890"
