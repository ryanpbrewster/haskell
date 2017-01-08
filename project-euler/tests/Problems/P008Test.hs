module Problems.P008Test (case_008_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P008

case_008_main :: Assertion
case_008_main = process "foo" @?= "233168"
