module Problems.P025Test (case_025_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P025

case_025_main :: Assertion
case_025_main = solve @?= "233168"
