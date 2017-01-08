module Problems.P029Test (case_029_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P029

case_029_main :: Assertion
case_029_main = solve @?= "9183"
