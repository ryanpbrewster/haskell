module Problems.P188Test (case_188_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P188

case_188_main :: Assertion
case_188_main = solve @?= "233168"
