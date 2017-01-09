module Problems.P032Test
  ( case_032_main
  ) where

import Problems.P032
import Test.Tasty.Discover (Assertion, (@?=))

case_032_main :: Assertion
case_032_main = solve @?= "45228"
