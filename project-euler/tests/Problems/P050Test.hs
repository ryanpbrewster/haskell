module Problems.P050Test
  ( case_050_main
  ) where

import Problems.P050
import Test.Tasty.Discover (Assertion, (@?=))

case_050_main :: Assertion
case_050_main = solve @?= "233168"
