module Problems.P050Spec (spec) where

import Test.Hspec
import Problems.P050

spec :: Spec
spec = do
  describe "problem 050" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

