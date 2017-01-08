module Problems.P147Test (case_147_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P147

case_147_main :: Assertion
case_147_main = solve @?= "233168"
