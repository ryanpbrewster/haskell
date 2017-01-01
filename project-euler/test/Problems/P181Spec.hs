module Problems.P181Spec (spec) where

import Test.Hspec
import Problems.P181

spec :: Spec
spec = do
  describe "problem 181" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

