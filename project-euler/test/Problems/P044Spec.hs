module Problems.P044Spec (spec) where

import Test.Hspec
import Problems.P044

spec :: Spec
spec = do
  describe "problem 044" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

