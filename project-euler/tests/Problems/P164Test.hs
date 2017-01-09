module Problems.P164Test
  ( case_164_main
  ) where

import Problems.P164
import Test.Tasty.Discover (Assertion, (@?=))

case_164_main :: Assertion
case_164_main = solve @?= "233168"
