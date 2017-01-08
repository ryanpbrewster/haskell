module Problems.P022Test (case_022_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P022

case_022_main :: Assertion
case_022_main = process "foo" @?= "233168"
