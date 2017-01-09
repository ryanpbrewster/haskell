module Problems.P190Test
  ( case_190_main
  ) where

import Problems.P190
import Test.Tasty.Discover (Assertion, (@?=))

case_190_main :: Assertion
case_190_main = solve @?= "233168"
