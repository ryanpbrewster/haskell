module Problems.P240Spec (spec) where

import Test.Hspec
import Problems.P240

spec :: Spec
spec = do
  describe "problem 240" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

