module Problems.P164Test (case_164_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P164

case_164_main :: Assertion
case_164_main = solve @?= "233168"
