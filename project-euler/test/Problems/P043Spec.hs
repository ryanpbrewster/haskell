module Problems.P043Spec (spec) where

import Test.Hspec
import Problems.P043

spec :: Spec
spec = do
  describe "problem 043" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

