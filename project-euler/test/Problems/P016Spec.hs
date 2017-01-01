module Problems.P016Spec (spec) where

import Test.Hspec
import Problems.P016

spec :: Spec
spec = do
  describe "problem 016" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

