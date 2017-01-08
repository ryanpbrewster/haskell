module Problems.P049Test (case_049_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P049

case_049_main :: Assertion
case_049_main = solve @?= "233168"
