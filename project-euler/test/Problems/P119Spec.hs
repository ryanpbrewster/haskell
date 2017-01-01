module Problems.P119Spec (spec) where

import Test.Hspec
import Problems.P119

spec :: Spec
spec = do
  describe "problem 119" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

