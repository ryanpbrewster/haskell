module Problems.P099Test (case_099_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P099

case_099_main :: Assertion
case_099_main = process "foo" @?= "233168"
