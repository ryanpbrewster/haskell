module Problems.P077Spec (spec) where

import Test.Hspec
import Problems.P077

spec :: Spec
spec = do
  describe "problem 077" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

