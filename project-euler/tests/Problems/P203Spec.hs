module Problems.P203Spec (spec) where

import Test.Hspec
import Problems.P203

spec :: Spec
spec = do
  describe "problem 203" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

