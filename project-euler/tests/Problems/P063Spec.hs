module Problems.P063Spec (spec) where

import Test.Hspec
import Problems.P063

spec :: Spec
spec = do
  describe "problem 063" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

