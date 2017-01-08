module Problems.P067Test (case_067_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P067

case_067_main :: Assertion
case_067_main = process "foo" @?= "233168"
