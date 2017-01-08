module Problems.P129Test (case_129_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P129

case_129_main :: Assertion
case_129_main = solve @?= "233168"
