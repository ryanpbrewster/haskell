module Problems.P129Spec (spec) where

import Test.Hspec
import Problems.P129

spec :: Spec
spec = do
  describe "problem 129" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

