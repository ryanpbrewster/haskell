module Problems.P002Test (case_002_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P002

case_002_main :: Assertion
case_002_main = solve @?= "233168"
