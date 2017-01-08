module Problems.P054Test (case_054_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P054

case_054_main :: Assertion
case_054_main = process "foo" @?= "233168"
