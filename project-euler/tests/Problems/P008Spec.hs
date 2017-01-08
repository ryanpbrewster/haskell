module Problems.P008Spec (spec) where

import Test.Hspec
import Problems.P008

spec :: Spec
spec = do
  describe "problem 008" $ do
    it "produces the correct answer" $ do
      process "foo" `shouldBe` "233168"
