module Problems.P119Test (case_119_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P119

case_119_main :: Assertion
case_119_main = solve @?= "233168"
