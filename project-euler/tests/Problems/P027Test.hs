module Problems.P027Test (case_027_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P027

case_027_main :: Assertion
case_027_main = solve @?= "233168"
