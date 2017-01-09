module Problems.P003Test
  ( case_003_main
  ) where

import Problems.P003
import Test.Tasty.Discover (Assertion, (@?=))

case_003_main :: Assertion
case_003_main = solve @?= "6857"
