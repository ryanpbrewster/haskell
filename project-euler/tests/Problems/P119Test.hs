module Problems.P119Test
  ( case_119_main
  ) where

import Problems.P119
import Test.Tasty.Discover (Assertion, (@?=))

case_119_main :: Assertion
case_119_main = solve @?= "248155780267521"
