module Problems.P041Test (case_041_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P041

case_041_main :: Assertion
case_041_main = solve @?= "7652413"
