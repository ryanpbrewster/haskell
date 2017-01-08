module Problems.P042Test (case_042_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P042

case_042_main :: Assertion
case_042_main = process "foo" @?= "233168"
