module Problems.P016Test
  ( case_016_main
  ) where

import Problems.P016
import Test.Tasty.Discover (Assertion, (@?=))

case_016_main :: Assertion
case_016_main = solve @?= "1366"
