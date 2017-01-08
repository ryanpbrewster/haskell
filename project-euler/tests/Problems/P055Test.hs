module Problems.P055Test (case_055_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P055

case_055_main :: Assertion
case_055_main = solve @?= "233168"
