module Problems.P045Spec (spec) where

import Test.Hspec
import Problems.P045

spec :: Spec
spec = do
  describe "problem 045" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

