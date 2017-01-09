module Problems.P024Test
  ( case_024_main
  ) where

import Problems.P024
import Test.Tasty.Discover (Assertion, (@?=))

case_024_main :: Assertion
case_024_main = solve @?= "2783915460"
