module Problems.P015Test (case_015_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P015

case_015_main :: Assertion
case_015_main = solve @?= "137846528820"
