module Problems.P058Spec (spec) where

import Test.Hspec
import Problems.P058

spec :: Spec
spec = do
  describe "problem 058" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

