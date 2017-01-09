module Problems.P099Test
  ( case_099_main
  ) where

import Problems.P099
import Test.Tasty.Discover (Assertion, (@?=))

case_099_main :: Assertion
case_099_main = process "foo" @?= "233168"
