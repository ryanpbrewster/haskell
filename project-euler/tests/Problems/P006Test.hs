module Problems.P006Test
  ( case_006_main
  ) where

import Problems.P006
import Test.Tasty.Discover (Assertion, (@?=))

case_006_main :: Assertion
case_006_main = solve @?= "25164150"
