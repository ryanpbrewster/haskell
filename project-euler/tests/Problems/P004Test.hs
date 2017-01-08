module Problems.P004Test (case_004_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P004

case_004_main :: Assertion
case_004_main = solve @?= "233168"
