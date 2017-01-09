module Problems.P052Test
  ( case_052_main
  ) where

import Problems.P052
import Test.Tasty.Discover (Assertion, (@?=))

case_052_main :: Assertion
case_052_main = solve @?= "142857"
