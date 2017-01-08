module Problems.P072Test (case_072_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P072

case_072_main :: Assertion
case_072_main = solve @?= "233168"
