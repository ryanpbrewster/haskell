module Problems.P011Spec (spec) where

import Test.Hspec
import Problems.P011

spec :: Spec
spec = do
  describe "problem 011" $ do
    it "produces the correct answer" $ do
      process "foo" `shouldBe` "233168"
