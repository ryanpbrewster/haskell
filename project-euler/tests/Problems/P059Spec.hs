module Problems.P059Spec (spec) where

import Test.Hspec
import Problems.P059

spec :: Spec
spec = do
  describe "problem 059" $ do
    it "produces the correct answer" $ do
      process "foo" `shouldBe` "233168"
