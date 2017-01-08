module Problems.P026Test (case_026_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P026

case_026_main :: Assertion
case_026_main = solve @?= "233168"
