module Problems.P049Test
  ( case_049_main
  ) where

import Problems.P049
import Test.Tasty.Discover (Assertion, (@?=))

case_049_main :: Assertion
case_049_main = solve @?= "296962999629"
