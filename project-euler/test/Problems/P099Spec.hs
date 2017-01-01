module Problems.P099Spec (spec) where

import Test.Hspec
import Problems.P099

spec :: Spec
spec = do
  describe "problem 099" $ do
    it "produces the correct answer" $ do
      process "placeholder" `shouldBe` "233168"

