module Problems.P065Test
  ( case_065_main
  ) where

import Problems.P065
import Test.Tasty.Discover (Assertion, (@?=))

case_065_main :: Assertion
case_065_main = solve @?= "272"
