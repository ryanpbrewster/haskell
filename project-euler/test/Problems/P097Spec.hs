module Problems.P097Spec (spec) where

import Test.Hspec
import Problems.P097

spec :: Spec
spec = do
  describe "problem 097" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

