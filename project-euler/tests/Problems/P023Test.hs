module Problems.P023Test (case_023_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P023

case_023_main :: Assertion
case_023_main = solve @?= "233168"
