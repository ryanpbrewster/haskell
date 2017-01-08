module Problems.P053Test (case_053_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P053

case_053_main :: Assertion
case_053_main = solve @?= "233168"
