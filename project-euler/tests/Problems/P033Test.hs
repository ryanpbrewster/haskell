module Problems.P033Test (case_033_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P033

case_033_main :: Assertion
case_033_main = solve @?= "233168"
