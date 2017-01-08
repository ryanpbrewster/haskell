module Problems.P112Test (case_112_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P112

case_112_main :: Assertion
case_112_main = solve @?= "233168"
