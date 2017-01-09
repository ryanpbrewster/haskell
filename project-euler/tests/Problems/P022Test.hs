module Problems.P022Test
  ( case_022_main
  ) where

import Problems.P022
import Test.Tasty.Discover (Assertion, (@?=))

import qualified TestData.P022

case_022_main :: Assertion
case_022_main = process TestData.P022.txt @?= "871198282"
