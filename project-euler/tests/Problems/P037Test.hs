module Problems.P037Test (case_037_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P037

case_037_main :: Assertion
case_037_main = solve @?= "748317"
