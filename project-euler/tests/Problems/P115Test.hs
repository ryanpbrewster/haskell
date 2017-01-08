module Problems.P115Test (case_115_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P115

case_115_main :: Assertion
case_115_main = solve @?= "233168"
