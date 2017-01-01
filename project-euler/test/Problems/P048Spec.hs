module Problems.P048Spec (spec) where

import Test.Hspec
import Problems.P048

spec :: Spec
spec = do
  describe "problem 048" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

