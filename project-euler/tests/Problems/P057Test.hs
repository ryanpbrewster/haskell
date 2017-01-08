module Problems.P057Test (case_057_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P057

case_057_main :: Assertion
case_057_main = solve @?= "233168"
