module Problems.P020Test (case_020_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P020

case_020_main :: Assertion
case_020_main = solve @?= "233168"
