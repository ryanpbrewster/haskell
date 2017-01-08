module Problems.P059Test (case_059_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P059

case_059_main :: Assertion
case_059_main = process "foo" @?= "233168"
