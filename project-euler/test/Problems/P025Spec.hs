module Problems.P025Spec (spec) where

import Test.Hspec
import Problems.P025

spec :: Spec
spec = do
  describe "problem 025" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

