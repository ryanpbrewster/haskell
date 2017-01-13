module Problems.P097Test
  ( case_097_main
  ) where

import Problems.P097
import Test.Tasty.Discover (Assertion, (@?=))

case_097_main :: Assertion
case_097_main = solve @?= "8739992577"
