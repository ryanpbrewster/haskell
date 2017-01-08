module Problems.P101Spec (spec) where

import Test.Hspec
import Problems.P101

spec :: Spec
spec = do
  describe "problem 101" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

