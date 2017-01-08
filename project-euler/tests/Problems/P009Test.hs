module Problems.P009Test (case_009_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P009

case_009_main :: Assertion
case_009_main = solve @?= "31875000"
