module Problems.P037Test
  ( case_037_main
  ) where

import Problems.P037
import Test.Tasty.Discover (Assertion, (@?=))

case_037_main :: Assertion
case_037_main = solve @?= "748317"
