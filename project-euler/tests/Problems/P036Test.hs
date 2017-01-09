module Problems.P036Test
  ( case_036_main
  ) where

import Problems.P036
import Test.Tasty.Discover (Assertion, (@?=))

case_036_main :: Assertion
case_036_main = solve @?= "872187"
