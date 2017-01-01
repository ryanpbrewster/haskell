module Problems.P116Spec (spec) where

import Test.Hspec
import Problems.P116

spec :: Spec
spec = do
  describe "problem 116" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

