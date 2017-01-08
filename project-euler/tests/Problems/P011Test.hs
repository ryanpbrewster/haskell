module Problems.P011Test (case_011_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P011

case_011_main :: Assertion
case_011_main = process "foo" @?= "233168"
