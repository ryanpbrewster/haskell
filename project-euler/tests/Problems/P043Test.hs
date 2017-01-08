module Problems.P043Test (case_043_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P043

case_043_main :: Assertion
case_043_main = solve @?= "233168"
