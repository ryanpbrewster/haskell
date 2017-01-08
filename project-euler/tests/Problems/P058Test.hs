module Problems.P058Test (case_058_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P058

case_058_main :: Assertion
case_058_main = solve @?= "233168"
