module Problems.P200Spec (spec) where

import Test.Hspec
import Problems.P200

spec :: Spec
spec = do
  describe "problem 200" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

