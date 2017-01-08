module Problems.P065Test (case_065_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P065

case_065_main :: Assertion
case_065_main = solve @?= "233168"
