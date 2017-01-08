module Problems.P017Test (case_017_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P017

case_017_main :: Assertion
case_017_main = solve @?= "21124"
