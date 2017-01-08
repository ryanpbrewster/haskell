module Problems.P010Spec (spec) where

import Test.Hspec
import Problems.P010

spec :: Spec
spec = do
  describe "problem 010" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

