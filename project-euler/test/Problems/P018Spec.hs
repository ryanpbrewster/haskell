module Problems.P018Spec (spec) where

import Test.Hspec
import Problems.P018

spec :: Spec
spec = do
  describe "problem 018" $ do
    it "produces the correct answer" $ do
      process "foo" `shouldBe` "233168"
