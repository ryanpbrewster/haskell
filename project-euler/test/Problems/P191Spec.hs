module Problems.P191Spec (spec) where

import Test.Hspec
import Problems.P191

spec :: Spec
spec = do
  describe "problem 191" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

