module Problems.P067Spec (spec) where

import Test.Hspec
import Problems.P067

spec :: Spec
spec = do
  describe "problem 067" $ do
    it "produces the correct answer" $ do
      process "placeholder" `shouldBe` "233168"
