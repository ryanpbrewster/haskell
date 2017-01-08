module Problems.P016Test (case_016_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P016

case_016_main :: Assertion
case_016_main = solve @?= "1366"
