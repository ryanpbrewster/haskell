module Problems.P112Spec (spec) where

import Test.Hspec
import Problems.P112

spec :: Spec
spec = do
  describe "problem 112" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

