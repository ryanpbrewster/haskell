module Problems.P036Test (case_036_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P036

case_036_main :: Assertion
case_036_main = solve @?= "233168"
