module Problems.P038Spec (spec) where

import Test.Hspec
import Problems.P038

spec :: Spec
spec = do
  describe "problem 038" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

