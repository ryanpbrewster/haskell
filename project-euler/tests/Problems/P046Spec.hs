module Problems.P046Spec (spec) where

import Test.Hspec
import Problems.P046

spec :: Spec
spec = do
  describe "problem 046" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

