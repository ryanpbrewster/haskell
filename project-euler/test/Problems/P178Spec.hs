module Problems.P178Spec (spec) where

import Test.Hspec
import Problems.P178

spec :: Spec
spec = do
  describe "problem 178" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

