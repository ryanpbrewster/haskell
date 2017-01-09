module Problems.P079Test
  ( case_079_main
  ) where

import Problems.P079
import Test.Tasty.Discover (Assertion, (@?=))

case_079_main :: Assertion
case_079_main = process "foo" @?= "233168"
