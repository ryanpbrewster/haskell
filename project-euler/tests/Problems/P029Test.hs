module Problems.P029Test
  ( case_029_main
  ) where

import Problems.P029
import Test.Tasty.Discover (Assertion, (@?=))

case_029_main :: Assertion
case_029_main = solve @?= "9183"
