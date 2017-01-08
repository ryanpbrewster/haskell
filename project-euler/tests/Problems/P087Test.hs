module Problems.P087Test (case_087_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P087

case_087_main :: Assertion
case_087_main = solve @?= "233168"
