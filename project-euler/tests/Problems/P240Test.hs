module Problems.P240Test (case_240_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P240

case_240_main :: Assertion
case_240_main = solve @?= "233168"
