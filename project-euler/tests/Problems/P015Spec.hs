module Problems.P015Spec (spec) where

import Test.Hspec
import Problems.P015

spec :: Spec
spec = do
  describe "problem 015" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

