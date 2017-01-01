module Problems.P004Spec (spec) where

import Test.Hspec
import Problems.P004

spec :: Spec
spec = do
  describe "problem 004" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

