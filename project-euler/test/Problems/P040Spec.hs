module Problems.P040Spec (spec) where

import Test.Hspec
import Problems.P040

spec :: Spec
spec = do
  describe "problem 040" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

