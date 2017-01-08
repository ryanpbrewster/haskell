module Problems.P178Test (case_178_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P178

case_178_main :: Assertion
case_178_main = solve @?= "233168"
