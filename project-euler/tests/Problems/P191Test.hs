module Problems.P191Test (case_191_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P191

case_191_main :: Assertion
case_191_main = solve @?= "233168"
