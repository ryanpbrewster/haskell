module Problems.P201Test (case_201_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P201

case_201_main :: Assertion
case_201_main = solve @?= "233168"
