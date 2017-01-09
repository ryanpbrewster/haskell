module Problems.P045Test
  ( case_045_main
  ) where

import Problems.P045
import Test.Tasty.Discover (Assertion, (@?=))

case_045_main :: Assertion
case_045_main = solve @?= "1533776805"
