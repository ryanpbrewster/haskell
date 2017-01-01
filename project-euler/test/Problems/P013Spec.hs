module Problems.P013Spec (spec) where

import Test.Hspec
import Problems.P013

spec :: Spec
spec = do
  describe "problem 013" $ do
    it "produces the correct answer" $ do
      process "foo" `shouldBe` "233168"
