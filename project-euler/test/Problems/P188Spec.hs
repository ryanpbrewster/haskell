module Problems.P188Spec (spec) where

import Test.Hspec
import Problems.P188

spec :: Spec
spec = do
  describe "problem 188" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

