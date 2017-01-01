module Problems.P150Spec (spec) where

import Test.Hspec
import Problems.P150

spec :: Spec
spec = do
  describe "problem 150" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

