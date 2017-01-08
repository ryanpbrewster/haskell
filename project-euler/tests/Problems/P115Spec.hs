module Problems.P115Spec (spec) where

import Test.Hspec
import Problems.P115

spec :: Spec
spec = do
  describe "problem 115" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

