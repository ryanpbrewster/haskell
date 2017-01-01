module Problems.P197Spec (spec) where

import Test.Hspec
import Problems.P197

spec :: Spec
spec = do
  describe "problem 197" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

