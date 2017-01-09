module Problems.P007Test
  ( case_007_main
  ) where

import Problems.P007
import Test.Tasty.Discover (Assertion, (@?=))

case_007_main :: Assertion
case_007_main = solve @?= "104743"
