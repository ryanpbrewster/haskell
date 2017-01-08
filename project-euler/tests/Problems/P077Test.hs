module Problems.P077Test (case_077_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P077

case_077_main :: Assertion
case_077_main = solve @?= "233168"
