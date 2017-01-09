module Problems.P055Test
  ( case_055_main
  ) where

import Problems.P055
import Test.Tasty.Discover (Assertion, (@?=))

case_055_main :: Assertion
case_055_main = solve @?= "249"
