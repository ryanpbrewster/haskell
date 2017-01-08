module Problems.P249Test (case_249_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P249

case_249_main :: Assertion
case_249_main = solve @?= "233168"
