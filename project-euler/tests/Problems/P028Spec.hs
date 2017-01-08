module Problems.P028Spec (spec) where

import Test.Hspec
import Problems.P028

spec :: Spec
spec = do
  describe "problem 028" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

