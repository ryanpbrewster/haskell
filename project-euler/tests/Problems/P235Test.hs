module Problems.P235Test (case_235_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P235

case_235_main :: Assertion
case_235_main = solve @?= "233168"
