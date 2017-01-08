module Problems.P032Test (case_032_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P032

case_032_main :: Assertion
case_032_main = solve @?= "233168"
