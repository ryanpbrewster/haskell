module Problems.P033Test
  ( case_033_main
  ) where

import Problems.P033
import Test.Tasty.Discover (Assertion, (@?=))

case_033_main :: Assertion
case_033_main = solve @?= "100"
