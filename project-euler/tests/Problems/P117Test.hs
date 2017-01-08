module Problems.P117Test (case_117_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P117

case_117_main :: Assertion
case_117_main = solve @?= "233168"
