module Problems.P209Test (case_209_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P209

case_209_main :: Assertion
case_209_main = solve @?= "233168"
