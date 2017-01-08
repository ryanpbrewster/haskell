module Problems.P021Spec (spec) where

import Test.Hspec
import Problems.P021

spec :: Spec
spec = do
  describe "problem 021" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

