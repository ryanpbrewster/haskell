module Problems.P062Test
  ( case_062_main
  ) where

import Problems.P062
import Test.Tasty.Discover (Assertion, (@?=))

case_062_main :: Assertion
case_062_main = solve @?= "233168"
