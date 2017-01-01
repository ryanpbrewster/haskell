module Problems.P054Spec (spec) where

import Test.Hspec
import Problems.P054

spec :: Spec
spec = do
  describe "problem 054" $ do
    it "produces the correct answer" $ do
      process "foo" `shouldBe` "233168"
