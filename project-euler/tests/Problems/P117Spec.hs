module Problems.P117Spec (spec) where

import Test.Hspec
import Problems.P117

spec :: Spec
spec = do
  describe "problem 117" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

