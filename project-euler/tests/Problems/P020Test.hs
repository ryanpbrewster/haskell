module Problems.P020Test
  ( case_020_main
  ) where

import Problems.P020
import Test.Tasty.Discover (Assertion, (@?=))

case_020_main :: Assertion
case_020_main = solve @?= "648"
