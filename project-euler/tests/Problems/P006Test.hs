module Problems.P006Test (case_006_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P006

case_006_main :: Assertion
case_006_main = solve @?= "25164150"
