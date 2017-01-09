module Problems.P046Test
  ( case_046_main
  ) where

import Problems.P046
import Test.Tasty.Discover (Assertion, (@?=))

case_046_main :: Assertion
case_046_main = solve @?= "5777"
