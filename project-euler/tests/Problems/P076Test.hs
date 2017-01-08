module Problems.P076Test (case_076_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P076

case_076_main :: Assertion
case_076_main = solve @?= "233168"
