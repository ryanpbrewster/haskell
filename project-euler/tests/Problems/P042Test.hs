module Problems.P042Test (case_042_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P042

import qualified TestData.P042

case_042_main :: Assertion
case_042_main = process TestData.P042.txt@?= "162"
