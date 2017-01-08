module Problems.P079Test (case_079_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P079

case_079_main :: Assertion
case_079_main = process "foo" @?= "233168"
