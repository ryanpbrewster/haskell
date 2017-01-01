module Problems.P206Spec (spec) where

import Test.Hspec
import Problems.P206

spec :: Spec
spec = do
  describe "problem 206" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

