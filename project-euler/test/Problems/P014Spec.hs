module Problems.P014Spec (spec) where

import Test.Hspec
import Problems.P014

spec :: Spec
spec = do
  describe "problem 014" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

