module Problems.P031Spec (spec) where

import Test.Hspec
import Problems.P031

spec :: Spec
spec = do
  describe "problem 031" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

