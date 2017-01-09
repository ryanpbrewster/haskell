module Problems.P059Test
  ( case_059_main
  ) where

import Problems.P059
import Test.Tasty.Discover (Assertion, (@?=))

case_059_main :: Assertion
case_059_main = process "foo" @?= "233168"
