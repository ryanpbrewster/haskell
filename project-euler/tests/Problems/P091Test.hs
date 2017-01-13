module Problems.P091Test
  ( case_091_main
  ) where

import Problems.P091
import Test.Tasty.Discover (Assertion, (@?=))

case_091_main :: Assertion
case_091_main = solve @?= "14234"
