module Problems.P038Test
  ( case_038_main
  ) where

import Problems.P038
import Test.Tasty.Discover (Assertion, (@?=))

case_038_main :: Assertion
case_038_main = solve @?= "932718654"
