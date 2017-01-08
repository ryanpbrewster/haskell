module Problems.P003Test (case_003_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P003

case_003_main :: Assertion
case_003_main = solve @?= "6857"
