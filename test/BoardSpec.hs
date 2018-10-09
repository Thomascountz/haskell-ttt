module BoardSpec (spec) where

import Test.Hspec
import Board

spec :: Spec
spec =  do
  describe "initBoard" $ do
    it "returns a board with a length of 9" $
      length initBoard `shouldBe` 9
