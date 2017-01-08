module Problems.P013Test (case_013_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P013

case_013_main :: Assertion
case_013_main = process "foo" @?= "233168"
