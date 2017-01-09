module Problems.P157Test
  ( case_157_main
  ) where

import Problems.P157
import Test.Tasty.Discover (Assertion, (@?=))

case_157_main :: Assertion
case_157_main = solve @?= "233168"
