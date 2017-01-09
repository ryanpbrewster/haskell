module Problems.P004Test
  ( case_004_main
  ) where

import Problems.P004
import Test.Tasty.Discover (Assertion, (@?=))

case_004_main :: Assertion
case_004_main = solve @?= "906609"
