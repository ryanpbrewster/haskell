module Problems.P204Spec (spec) where

import Test.Hspec
import Problems.P204

spec :: Spec
spec = do
  describe "problem 204" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

