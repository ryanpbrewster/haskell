module Problems.P054Test
  ( case_054_main
  ) where

import Problems.P054
import Test.Tasty.Discover (Assertion, (@?=))

case_054_main :: Assertion
case_054_main = process "foo" @?= "233168"
