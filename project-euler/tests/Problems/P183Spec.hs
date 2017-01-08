module Problems.P183Spec (spec) where

import Test.Hspec
import Problems.P183

spec :: Spec
spec = do
  describe "problem 183" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

