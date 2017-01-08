module Problems.P010Test (case_010_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P010

case_010_main :: Assertion
case_010_main = solve @?= "233168"
