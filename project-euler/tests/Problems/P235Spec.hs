module Problems.P235Spec (spec) where

import Test.Hspec
import Problems.P235

spec :: Spec
spec = do
  describe "problem 235" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

