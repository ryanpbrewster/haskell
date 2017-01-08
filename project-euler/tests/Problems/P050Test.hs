module Problems.P050Test (case_050_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P050

case_050_main :: Assertion
case_050_main = solve @?= "233168"
