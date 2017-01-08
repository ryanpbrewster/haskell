module Problems.P201Spec (spec) where

import Test.Hspec
import Problems.P201

spec :: Spec
spec = do
  describe "problem 201" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

