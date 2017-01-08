module Problems.P097Test (case_097_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P097

case_097_main :: Assertion
case_097_main = solve @?= "233168"
