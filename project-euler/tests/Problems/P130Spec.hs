module Problems.P130Spec (spec) where

import Test.Hspec
import Problems.P130

spec :: Spec
spec = do
  describe "problem 130" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

