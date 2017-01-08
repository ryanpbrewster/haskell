module Problems.P183Test (case_183_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P183

case_183_main :: Assertion
case_183_main = solve @?= "233168"
