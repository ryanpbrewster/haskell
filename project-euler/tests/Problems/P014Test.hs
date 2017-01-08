module Problems.P014Test (case_014_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P014

case_014_main :: Assertion
case_014_main = solve @?= "233168"
