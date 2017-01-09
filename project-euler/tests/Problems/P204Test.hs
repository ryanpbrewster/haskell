module Problems.P204Test
  ( case_204_main
  ) where

import Problems.P204
import Test.Tasty.Discover (Assertion, (@?=))

case_204_main :: Assertion
case_204_main = solve @?= "233168"
