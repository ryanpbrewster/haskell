module Problems.P079Spec (spec) where

import Test.Hspec
import Problems.P079

spec :: Spec
spec = do
  describe "problem 079" $ do
    it "produces the correct answer" $ do
      process "placeholder" `shouldBe` "233168"

