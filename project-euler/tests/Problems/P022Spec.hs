module Problems.P022Spec (spec) where

import Test.Hspec
import Problems.P022

spec :: Spec
spec = do
  describe "problem 022" $ do
    it "produces the correct answer" $ do
      process "foo" `shouldBe` "233168"
