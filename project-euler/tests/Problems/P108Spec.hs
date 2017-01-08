module Problems.P108Spec (spec) where

import Test.Hspec
import Problems.P108

spec :: Spec
spec = do
  describe "problem 108" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

