module Problems.P087Spec (spec) where

import Test.Hspec
import Problems.P087

spec :: Spec
spec = do
  describe "problem 087" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

