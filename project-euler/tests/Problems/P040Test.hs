module Problems.P040Test (case_040_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P040

case_040_main :: Assertion
case_040_main = solve @?= "210"
