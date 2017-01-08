module Problems.P020Spec (spec) where

import Test.Hspec
import Problems.P020

spec :: Spec
spec = do
  describe "problem 020" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

