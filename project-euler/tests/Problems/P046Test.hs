module Problems.P046Test (case_046_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P046

case_046_main :: Assertion
case_046_main = solve @?= "233168"
