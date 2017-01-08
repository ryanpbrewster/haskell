module Problems.P150Test (case_150_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P150

case_150_main :: Assertion
case_150_main = solve @?= "233168"
