module Problems.P067Test
  ( case_067_main
  ) where

import Problems.P067
import Test.Tasty.Discover (Assertion, (@?=))

case_067_main :: Assertion
case_067_main = process "foo" @?= "233168"
