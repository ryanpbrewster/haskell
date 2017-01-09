module Problems.P149Test
  ( case_149_main
  ) where

import Problems.P149
import Test.Tasty.Discover (Assertion, (@?=))

case_149_main :: Assertion
case_149_main = solve @?= "233168"
