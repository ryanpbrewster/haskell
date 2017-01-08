module Problems.P200Test (case_200_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P200

case_200_main :: Assertion
case_200_main = solve @?= "233168"
